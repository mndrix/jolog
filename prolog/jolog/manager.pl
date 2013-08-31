:- module(jolog_manager, [ manager_loop/2 ]).

manager_loop(Module, Outstanding) :-
    debug(jolog, '~w', [manager(try_matching_joins)]),
    ( Module:'$jolog_code' ->   % try matching join patterns
        manager_loop(Module, Outstanding)
    ; % otherwise ->
        debug(jolog, '~w', [manager(taking_event,Outstanding)]),
        take_event_no_block(Event),
        manager_loop(Module, Event, Outstanding)
    ).
manager_loop(Module, Event, Outstanding) :-
    debug(jolog,'~w',[manager(event, Event)]),
	( Event = send_message(Msg) ->
        jolog:assert(channels(Module,Msg)),
        manager_loop(Module, Outstanding)
    ; Event = active(N) ->
        Outstanding1 is Outstanding + N,
        take_event_no_block(Event1),
        manager_loop(Module, Event1, Outstanding1)
    ; Event = none ->
        ( Outstanding > 0 ->  % block until pending workers are done
            take_event_block(Event0),
            manager_loop(Module, Event0, Outstanding)
        ; true ->   % no chance of forward progress; stop Jolog
            manager_loop(Module, send_message(halt), Outstanding)
        )
    ; Event = halt ->
        true    % no more recursion
	).


% Takes the next event that's available for the manager thread.
% Blocks if there are no events available. Should only be called by
% the manager thread.
take_event_block(Event) :-
    thread_self(Self),
    thread_get_message(Self, Event).


% Like take_event_block but binds Event to 'none' instead of blocking.
% Should only be called by the manager thread.
take_event_no_block(Event) :-
    thread_self(Self),
    ( thread_get_message(Self, Message, [timeout(0)]) ->
        Event = Message
    ; % otherwise ->
        Event = none
    ).

