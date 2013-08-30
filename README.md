# Synopsis

    :- use_module(library(jolog)).
    % Prolog runs this predicate automatically
    main :-
        writeln('Starting Jolog'),
        % start Jolog, block until it's done
        start_jolog(user).

    % Jolog runs this pattern when it starts
    main &-
        writeln('In Jolog main'),
        then,
        ( send(hello)  % in one parallel process
        & sleep(1),    % in another parallel process
          send(world)
        ).

    % Once there are messages on both the 'hello' and 'world' channels
    world, hello &-
        writeln('Hello, World!').

which generates the following output with a small delay between the
second and third lines.

    Starting Jolog
    In Jolog main
    Hello, World!

# Description

WARNING: As suggested by the version number, this is early alpha software.
APIs and behavior are likely to change.

Jolog is an implementation of join patterns for Prolog that's
[inspired by JoCaml](http://jocaml.inria.fr/).  Join patterns provide
a clean, powerful way of thinking about concurrent and parallel
programming.

Jolog clauses are defined using the &-/2 operator.  Use it just like
the `:-/2` operator.  The clause's head contains a join pattern.  The
body contains processes and optional guards.

## Join Patterns

A join pattern is typically a conjunction (using `,/2`) of smaller
patterns.  Each small pattern is a term.  The term's name indicates a
channel.  The term's arguments indicate the content of a message on
that channel.  As you'd expect, patterns are matched via standard
Prolog unification.

So if our Jolog system has the following outstanding messages:

  * `temperature(75)`
  * `humidity(50)`
  * `air(overcast, windy)`

then all the following join patterns would match

  * `temperature(75)`
  * `temperature(T)` - binds `T` to `75`
  * `air(overcast,windy), humidity(H)` - binds `H` to `50`

## Guards

If a Jolog clause body contains the goal `then/0`, all goals occuring
before `then/0` are considered guards.  The guards typically examine
those bindings created by the head's join patterns.  All guard goals
must succeed before the matched messages are consumed.

For example, imagine we're programming a vending machine.  The
predicate `price(+Item, -Cost)` tells us how much it costs to purchase
a particular item.  The channel `balance/1` receives messages
indicating how much money has been deposited into the vending machine.
The channel `selected/1` receives a message when a customer presses an
item selection button on the vending machine.  We only want to
dispense an item if the balance exceeds the selected item's price.

    balance(Bal), selected(Item) &-
        price(Item, Cost),
        Bal >= Cost,
        then,
        ...

Before the ellided code (`...`) starts executing, the `balance/1` and
`selected/1` messages are consumed and no longer available to other
Jolog clauses.

If there is no `then/0` goal, it's the same as if the guard had been
`true`.

## Processes

Processes are concurrent computations.  They're executed after a join
pattern matches and all associated guards succeed.  They're
inexpensive to create, so don't worry about making thousands of them
if you need to.

A process can be any code that'd you pass to `call/1`.  The simplest
process is just a goal.  For example, in the `world, hello` pattern in
the Synopsis above, the process is `writeln('Hello, World!')`.  That
goal is executed concurrently and the enclosing Jolog clause doesn't
wait for it to complete.

It's often convenient to create multiple processes from a single Jolog
clause.  That's done with the &/2 operator.  Use this operator just
as you would use `;/2`.  For example the `main` Jolog clause in the
Synopsis creates two parallel processes.

Additional [examples are
available](https://github.com/mndrix/jolog/tree/master/examples).

## Messages

Messages are consumed via join patterns (described above).  They're
created by calling `send/1`.  You can send any term as a message, but
you'll get a warning if no Jolog clauses are listening for that
message.

# Changes in this Version

    * First public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(jolog).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/jolog

@author Michael Hendricks <michael@ndrix.org>
@license BSD
