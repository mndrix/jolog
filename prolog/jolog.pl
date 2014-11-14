:- module(jolog,[ op(1200,xfx,&-)
                , op(1100,xfy,&)
                , jolog_import_sentinel/0
                , send/1
                , start_jolog/1
                , start_jolog/2
                ]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(list_util), [split/3, xfy_list/3]).
:- use_module(library(lists), [same_length/2]).
:- use_module(library(error), [domain_error/2]).

:- use_module(library(jolog/manager)).


:- thread_local channels/2.  % channels(Module, Message)

/************* Jolog runtime code *******************/

%%	start_jolog(+Module,+Main) is det
%
%   Start running Jolog code defined in Module. After
%   creating the runtime environment, this predicate sends the Main
%   message. Use that message to trigger the rest
%   of your application. For example,
%
%       main :-  % starting point for Prolog
%           start_jolog(user, go).
%       go &-  % starting point for Jolog
%           ( one_process
%           & another_process
%           ).
%       ...
%
%  start_jolog/2 returns when one of the following is true:
%
%    * a message arrives on the halt/0 channel
%    * no join patterns match and no further progress can be made
start_jolog(Module,Main) :-
    % let workers know how they can reach the manager
    thread_self(Manager),
    set_meta(Module, manager, Manager),

    % create worker threads
    message_queue_create(Workers),
    set_meta(Module, workers, Workers),
    current_prolog_flag(cpu_count, CoreCount),
    WorkerCount is 2*CoreCount,
    %WorkerCount is 1 + CoreCount-CoreCount,
    set_meta(Module, worker_count, WorkerCount),
    forall( between(1,WorkerCount,_)
          , thread_create(worker_loop(Module, Workers), _, [detached(true)])
          ),

    % start manager loop
    Module:send(Main),
    manager_loop(Module).

%%	start_jolog(+Module) is det.
%
%	Like start_jolog/2 using `main` as the first message.
start_jolog(Module) :-
    start_jolog(Module, main).


%%	set_meta(+Module, +Name, +Value) is det.
%
%	Set a named meta value for a Jolog runtime defined in Module.
set_meta(Module, Name, Value) :-
    retractall(meta(Module,Name,_)),
    assertz(meta(Module,Name,Value)).


%%	meta(+Module, ?Name, ?Value)
%
%   True if Jolog runtime defined in Module has a named meta value with
%   Name and Value.
:- dynamic meta/3.


% called by macro-expanded Jolog code to spawn a new, parallel process.
% Should only be called by the manager thread.
spawn_process(Module, Process) :-
    debug(jolog, '~w', spawn_process(Module, Process)),

    % put process code in the workers' queue
    meta(Module, workers, Workers),
    thread_send_message(Workers, run_process(Process)),

    % notify the manager that one more worker is active
    meta(Module, manager, Manager),
    thread_send_message(Manager, active(+1)).


%%	send(+Message) is det
%
%   Sends a Jolog message to the relevant channel. Message should be a
%   ground term whose functor indicates the channel and whose arguments
%   indicate the message. For example, the following are legitimate
%   messages:
%
%       send(hello)         % hello/0 channel
%       send(hello(world))  % hello/1 channel
%       send(foo(alpha,beta,gamma,delta))  % foo/4 channel
:- meta_predicate send(:).
send(Module:Message) :-
    % someone listens on this channel; send the message
    functor(Message, Name, Arity),
    defined_channel(Module, Name, Arity),
    !,
    debug(jolog, '~w', [send(Module,Message)]),
    meta(Module, manager, Manager),
    thread_send_message(Manager, send_message(Message)).
send(Module:Message) :-
    % nobody listens on this channel; generate a warning
    print_message(warning, jolog_nobody_listening(Module, Message)).

% loop executed by Jolog worker threads
worker_loop(Module, Queue) :-
    debug(jolog,'~w',[worker(waiting)]),
    thread_get_message(Queue, Work),
    debug(jolog,'~w', [worker(job(Work))]),
    ( Work = halt ->
        debug(jolog,'worker exiting',[]),
        thread_exit(halt)
    ; Work = run_process(Goal) ->
        Module:ignore(Goal),
        meta(Module, manager, Manager),
        debug(jolog,'~w',[worker(finished)]),
        thread_send_message(Manager, active(-1))
    ; % otherwise ->
        domain_error(jolog_worker_message, Work)
    ),
    worker_loop(Module, Queue).



/*************************** Macro expansion code ***********************/

%%	jolog_import_sentinel
%
%   Nothing to see here. This is a junk predicate to keep Jolog macro
%   expansion isolated to those modules that want it.
jolog_import_sentinel.


% True if the currently loading module wants jolog macro expansion
wants_jolog_expansion :-
    prolog_load_context(module, Module),
    predicate_property(Module:jolog_import_sentinel, imported_from(jolog)).


% Parse a jolog clause into its constituent parts
parse_join_clause((Head &- Body), Patterns, Guards, Processes) :-
    % separate head into individual join patterns
    xfy_list(',', Head, Patterns),

    % separate body into guards and process terms
    xfy_list(',', Body, Goals),
    split(Goals, then, BodyParts),
    ( BodyParts = [Guards, ProcessTerms] ->
        true
    ; BodyParts = [ProcessTerms] ->  % missing 'then' goal
        Guards = []
    ),

    % build process goals from process terms
    ( ProcessTerms = [] ->
        Processes = []
    ; ProcessTerms = [ProcessDisjunction] ->
        xfy_list('&', ProcessDisjunction, Processes)
    ; % otherwise ->
        xfy_list(',', Process, ProcessTerms),
        Processes = [Process]
    ).


%%	build_peek_goal(+Pattern,-MessageRef,-PeekGoal)
%
%   Convert a jolog join pattern into a goal which checks whether the
%   pattern matches (PeekGoal). If calling PeekGoal succeeds, it binds
%   MessageRef to a reference to a clause which represents the matching
%   message. This reference can be used with erase/1 to consume the
%   message.
build_peek_goal(Module, Pattern, MessageRef, PeekGoal) :-
    PeekGoal = jolog:clause(channels(Module,Pattern), true, MessageRef).


%%	remember_channel(+Module,+JoinPattern)
%
%	Makes a note that Module has a join pattern which refers to a
%	specific channel.
:- dynamic defined_channel/3.
remember_channel(Module, Pattern) :-
    functor(Pattern, Name, Arity),
    ( defined_channel(Module,Name,Arity) ->
        true
    ; % otherwise ->
        assertz(defined_channel(Module,Name,Arity))
    ).


user:term_expansion((Head &- Body), ('$jolog_code' :- Goals)) :-
    wants_jolog_expansion,
    parse_join_clause((Head &- Body), Patterns, Guards, Processes),

    % build goals to peek at messages
    same_length(Patterns, MessageRefs),
    prolog_load_context(module, Module),
    maplist(build_peek_goal(Module), Patterns, MessageRefs, Peeks),

    % remember which channels have been defined
    prolog_load_context(module, Module),
    maplist(remember_channel(Module), Patterns),

    % build jolog clause body
    xfy_list(',', PeekGoals, Peeks),
    ( Guards=[] -> GuardGoals=true; xfy_list(',', GuardGoals, Guards) ),
    Module:dynamic('$jolog_code'/0),
    Goals = (
        debug(jolog,'Does head match? ~w', [Head]),
        PeekGoals,
        GuardGoals,
        !,
        maplist(erase, MessageRefs),
        maplist(jolog:spawn_process(Module), Processes)
    ).
user:term_expansion(end_of_file, _) :-
    % create Jolog clause to handle system halt
    wants_jolog_expansion,
    prolog_load_context(module, Module),

    % only add a 'halt' rule if there are other rules
    Module:once(clause('$jolog_code', _)),

    term_expansion((
        halt &-
             debug(jolog, 'halting', []),
             jolog:meta(Module, worker_count, WorkerCount),
             jolog:meta(Module, workers, Workers),
             jolog:meta(Module, manager, Manager),
             forall( between(1,WorkerCount,_)
                   , thread_send_message(Workers, halt)
                   ),
             thread_send_message(Manager, halt),
             then
    ), Clause),
    Module:asserta(Clause),  % halt clause goes first
    remember_channel(Module, halt),

    fail.  % let others have a chance to expand end_of_file

