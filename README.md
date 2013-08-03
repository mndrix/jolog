# Synopsis

    :- use_module(library(jolog)).
    main &-
        ( alpha
        ; beta
        ).
    alpha, beta &-
        writeln('Hello jolog').
    main :-
        run_jolog(user).

# Description

WARNING: early alpha software.  Documentation intentionally sparse
during alpha development.

Write concurrent and distributed systems with join patterns for Prolog.
Inspired by JoCaml and the join calculus.

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(jolog).

Source code available and pull requests accepted at
http://github.com/mndrix/jolog

@author Michael Hendricks <michael@ndrix.org>
@license BSD
