:- module(jolog,[ op(1200,xfx,&-)
                , jolog_import_sentinel/0
                ]).
:- use_module(library(list_util), [split/3, xfy_list/3]).
:- use_module(library(lists), [same_length/2]).

% junk predicate to let us create module-specific macros
jolog_import_sentinel.

wants_jolog_expansion :-
    prolog_load_context(module, Module),
    predicate_property(Module:jolog_import_sentinel, imported_from(jolog)).

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
        xfy_list(';', ProcessDisjunction, Processes)
    ; % otherwise ->
        xfy_list(',', Process, ProcessTerms),
        Processes = [Process]
    ).


build_peek_goal(Pattern, MessageRef, PeekGoal) :-
    PeekGoal = clause(Pattern, true, MessageRef).


:- dynamic defined_channel/3.
remember_channel(Module, Pattern) :-
    functor(Pattern, Name, Arity),
    ( defined_channel(Module,Name,Arity) ->
        true
    ; % otherwise ->
        assertz(defined_channel(Module,Name,Arity))
    ).


user:term_expansion((Head &- Body), (jolog :- Goals)) :-
    wants_jolog_expansion,
    parse_join_clause((Head &- Body), Patterns, Guards, Processes),

    % build goals to peek at messages
    same_length(Patterns, MessageRefs),
    maplist(build_peek_goal, Patterns, MessageRefs, Peeks),

    % remember which channels have been defined
    prolog_load_context(module, Module),
    maplist(remember_channel(Module), Patterns),

    % build jolog clause body
    xfy_list(',', PeekGoals, Peeks),
    ( Guards=[] -> GuardGoals=true; xfy_list(',', GuardGoals, Guards) ),
    Goals = (
        PeekGoals,
        GuardGoals,
        !,
        maplist(erase, MessageRefs),
        maplist(jolog:spawn_process(Module), Processes)
    ).
