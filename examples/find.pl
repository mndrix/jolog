#!/usr/bin/env pl
:- use_module(library(jolog)).
:- use_module(library(apply), [maplist/2]).
:- use_module(library(lambda)).
:- use_module(library(filesex), [directory_file_path/3]).
%:- debug(jolog).

main(Args) :-
    ( Args=[Start|_] -> true ; Start='.' ),
    start_jolog(user, entry(dir,Start)).


% don't descend into these directories
ignore_directory('.').
ignore_directory('..').
ignore_directory('.git').

% don't list these files
ignore_file(File) :-  % hidden files
    atom_concat('.',_,File).
ignore_file(File) :-  % backup files
    atom_concat(_,'~',File).


% Jolog definitions below here

% generic dir entry whose type we don't yet know
entry(Entry) &-
    ( exists_directory(Entry) -> % a directory
        file_base_name(Entry, Base),
        \+ ignore_directory(Base),
        send(entry(dir,Entry))
    ; true -> % a file
        send(entry(file,Entry))
    ).

% directories
entry(dir, Dir) &-
    directory_files(Dir, Entries),
    maplist(Dir+\E^(
        directory_file_path(Dir,E,Path),
        send(entry(Path))
    ),Entries).

% plain files
entry(file, File) &-
    file_base_name(File, Base),
    \+ ignore_file(Base),
    writeln(File).
