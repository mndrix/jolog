:- use_module(library(jolog)).

:- use_module(library(tap)).

% simplest possible join clause: one pattern, no guards, trivial process
jolog:parse_join_clause(
    (
        pattern_only &- true
    ),
    [pattern_only],
    [],
    [true]
).

% typical join clause with some of everything
jolog:parse_join_clause(
    (
        foo, bar(X) &-
            guard_one,
            guard_two(X),
            then,
            (   process_one(X)
            ;   process_two
            )
    ),
    [foo,bar(X)],
    [guard_one, guard_two(X)],
    [process_one(X), process_two]
).

% join clause with guards but no processes
jolog:parse_join_clause(
    (
        channel &- guard_me, then
    ),
    [channel],
    [guard_me],
    []
).
