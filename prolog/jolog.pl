:- module(jolog,[ op(1200,xfx,&-)
                , op(1100,xfy,&)
                , jolog_import_sentinel/0
                , send/1
                , start_jolog/1
                ]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(list_util), [split/3, xfy_list/3]).
:- use_module(library(lists), [same_length/2]).
:- use_module(library(error), [domain_error/2]).

/************* Jolog runtime code *******************/

%%	start_jolog(Module) is det
%
%   Start running Jolog code defined in Module. After
%   creating the runtime environment, this predicate sends a single
%   message to the main/0 channel. Use this channel to trigger the rest
%   of your application. For example,
%
%       main :-  % starting point for Prolog
%           start_jolog(user).
%       main &-  % starting point for Jolog
%           ( one_process
%           & another_process
%           ).
%       ...
%
%  start_jolog/1 returns as soon as a message is received on the halt/0
%  channel.
start_jolog(Module) :-
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
    % TODO make it a separate thread so it can do thread local assert/1
    Module:send(main),
    manager_loop(Module).


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
    meta(Module, workers, Workers),
    thread_send_message(Workers, run_process(Process)).


%%	send(+Message) is det
%
%   Sends a Jolog message to the relevant channel. Message should be a
%   ground term whose functor indicates the channel and whose arguments
%   indicatet the message. For example, the following are legitimate
%   messages:
%
%       send(hello)         % hello/0 channel
%       send(hello(world))  % hello/1 channel
%       send(foo(alpha,beta,gamma,delta))  % foo/4 channel
:- meta_predicate send(:).
:- thread_local channels/2.  % channels(Module, Message)
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


manager_loop(Module) :-
    debug(jolog, '~w', [manager(loop)]),
	take_event_no_block(Event),
	manager_loop(Module, Event).
manager_loop(Module, Event) :-
    debug(jolog,'~w',[manager(event, Event)]),
	( Event = send_message(Msg) ->
        assert(channels(Module,Msg)),
        manager_loop(Module)
	; Event = none ->
        debug(jolog, '~w', [manager(try_matching_joins)]),
		( Module:'jolog_code' ->  % try matching join patterns
			manager_loop(Module)
		; take_event_block(Event1) ->
			manager_loop(Module, Event1)
		)
    ; Event = halt ->
        true    % no more recursion
	).


% Takes the next event that's available for the manager thread. Event is
% none if there are no events available. This call does not
% block.  Should only be called by the manager thread.
take_event_no_block(Event) :-
    thread_self(Self),
    ( thread_get_message(Self, Message, [timeout(0)]) ->
        Event = Message
    ; % otherwise ->
        Event = none
    ).

% Like take_event_no_block/1 but blocks if there are no events
% available.  Should only be called by the manager thread.
take_event_block(Event) :-
    thread_self(Self),
    thread_get_message(Self, Event).


% loop executed by Jolog worker threads
worker_loop(Module, Queue) :-
    debug(jolog,'~w',[worker(loop)]),
    thread_get_message(Queue, Work),
    debug(jolog,'~w', [worker(Work)]),
    ( Work = halt ->
        thread_exit(halt)
    ; Work = run_process(Goal) ->
        Module:ignore(Goal)
    ; % otherwise ->
        domain_error(jolog_worker_message, Work)
    ),
    worker_loop(Module, Queue).


/************* Macro expansion code *******************/

% junk predicate to let us create module-specific macros
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


user:term_expansion((Head &- Body), ('jolog_code' :- Goals)) :-
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
    Module:dynamic('jolog_code'/0),
    Goals = (
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

    fail.  % let others have a chance to expand end_of_file

