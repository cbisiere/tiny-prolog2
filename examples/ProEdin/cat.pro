"Edinburgh demo: https://en.wikipedia.org/wiki/Prolog"

cat(tom).
animal(X) :- cat(X).

:- cat(tom).
:- cat(X).
:- animal(X).
