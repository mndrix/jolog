:- use_module(prolog/jolog).

foo, bar(X) &-
    guard_one,
    guard_two(X),
    then,
    (   process_one
    ;   process_two
    ).

pattern_only &- true.

:- use_module(library(tap)).

% add tests showing common usage
todo :- fail.
