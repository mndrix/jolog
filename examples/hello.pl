#!/usr/bin/env pl
:- use_module(library(jolog)).
%:- debug(jolog).

% Prolog runs this predicate automatically
main :-
    writeln('Starting jolog'),
    % start Jolog, block until it's done
    start_jolog(user).

% Jolog runs this pattern on start
main &-
    writeln('In jolog main'),
    then,
    ( send(hello)  % one parallel process
    & sleep(1),    % another parallel process
      send(world)
    ).

% Once there are messages on both the 'hello' and 'world' channels
world, hello &-
    writeln('Hello, World!').
