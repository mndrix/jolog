:- module(jolog_manager, [ manager_create/2
                         , manager_destroy/1
                         , manager_loop/1
                         ]).

manager_loop(Module) :-
    manager_loop(Module, 0).

manager_loop(Module, Outstanding) :-
    iterate_patterns(Module, Outstanding).


% create a Jolog manager
manager_create(Module,StartupMessage) :-
    message_queue_create(ManagerQueue),
    jolog:set_meta(Module, manager_queue, ManagerQueue),

    message_queue_create(WorkQueue),
    jolog:set_meta(Module, work_queue, WorkQueue),

    current_prolog_flag(cpu_count,CoreCount),
    WorkerCount is 2*CoreCount,
    jolog:set_meta(Module, worker_count, WorkerCount),
    length(Workers,WorkerCount),
    debug(jolog,"Jolog: starting ~d worker threads.",[WorkerCount]),
    maplist(jolog:create_worker(Module,WorkQueue),Workers),

    Module:send(StartupMessage).


% destroy a Jolog manager created with manager_create/2
manager_destroy(Module) :-
    retractall(jolog:meta(Module,_,_)),
    retractall(jolog:channels(_,_)).


% match as many join patterns as possible
iterate_patterns(Module, Outstanding) :-
    debug(jolog, '~w', [manager(iterate_patterns,Outstanding)]),
    Module:'$jolog_code',
    !,
    iterate_patterns(Module, Outstanding).
iterate_patterns(Module, Outstanding) :-
    iterate_events(Module, Outstanding).


% process as many manager events as possible
iterate_events(Module, Outstanding) :-
    take_event_no_block(Module,Event),
    debug(jolog,'~w',[manager(event, Event)]),
    handle_event(Event, Module, Outstanding).

handle_event(send_message(Msg), Module, Outstanding) :-
    debug(jolog,"Sending message: ~w",[Msg]),
    jolog:assert(channels(Module,Msg)),
    iterate_patterns(Module, Outstanding).  % patterns might match now
handle_event(active(N), Module, Outstanding0) :-
    Outstanding is Outstanding0 + N,
    debug(jolog,"Outstanding workers: ~d -> ~d",[Outstanding0,Outstanding]),
    iterate_events(Module, Outstanding).
handle_event(none, Module, Outstanding) :-
    ( Outstanding > 0 ->  % block until pending workers are done
        debug(jolog, 'manager blocking', []),
        take_event_block(Module,Event),
        handle_event(Event, Module, Outstanding)
    ; true ->   % no chance of forward progress; stop Jolog
        debug(jolog,"No events, no outstanding workers: sending halt",[]),
        handle_event(send_message(halt), Module, Outstanding)
    ).
handle_event(halt, _, _).  % no more recursion


% Takes the next event that's available for the manager thread.
% Blocks if there are no events available. Should only be called by
% the manager thread.
take_event_block(Module,Event) :-
    jolog:meta(Module,manager_queue,ManagerQueue),
    thread_get_message(ManagerQueue, Event).


% Like take_event_block but binds Event to 'none' instead of blocking.
% Should only be called by the manager thread.
take_event_no_block(Module,Event) :-
    jolog:meta(Module,manager_queue,ManagerQueue),
    ( thread_get_message(ManagerQueue, Message, [timeout(0)]) ->
        Event = Message
    ; % otherwise ->
        Event = none
    ).
