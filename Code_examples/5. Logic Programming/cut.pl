% Illustrate the effect of a cut.

foo(1).
foo(2).
bar(X,Y) :- foo(X), !, foo(Y).
bar(3,3).

% bar(X,2) :- !, foo(X).
% bar(3,3).


% Green cuts.

maxGreen(X,Y,Y) :- X < Y, !.
maxGreen(X,Y,X) :- X >= Y.


% Red cuts.

maxRed(X,Y,Y) :- X < Y, !.
maxRed(X,_,X).

lookupAny(Key, Val, [entry(Key,Val)|_]).
lookupAny(Key, Val, [_|Map]) :- lookupAny(Key, Val, Map).

lookupFirst(Key, Val, [entry(Key,Val)|_]) :- !.
lookupFirst(Key, Val, [_|Map]) :- lookupFirst(Key, Val, Map).

exampleMap(
  [ entry("x",3),
    entry("y",4),
    entry("z",5),
    entry("x",6),
    entry("z",7),
    entry("x",8) ]).


% Negation as failure.

not(P) :- P, !, fail.
not(_).


% Implementing conditional predicates

ifThenElse(Cond, Then, _) :- Cond, !, Then.
ifThenElse(_, _, Else) :- Else.

lookupOrDefault(Key, Default, Map, Result)
  :- ifThenElse(lookupFirst(Key, Val, Map), Result = Val, Result = Default).


% Illustrating negation

hobbit(frodo).
hobbit(sam).
hobbit(merry).
hobbit(pippin).

likes(frodo,ring).
likes(X,beer) :- hobbit(X), not(likes(X,ring)).
