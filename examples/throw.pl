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
    throw(oh_no_i_died).
