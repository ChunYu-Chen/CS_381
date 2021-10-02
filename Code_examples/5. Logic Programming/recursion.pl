% Structuring recursive rules so they don't loop on failure!


% Symmetric definition that loops on failure.
marriedBad(abe,mona).
marriedBad(clancy,jackie).
marriedBad(homer,marge).
marriedBad(X,Y) :- marriedBad(Y,X).


% Transitive definition that loops on failure.
ltBad(one,two).
ltBad(two,three).
ltBad(three,four).
ltBad(X,Z) :- ltBad(X,Y), ltBad(Y,Z).


% Symmetric definition that doesn't loop on failure.
marriedBasic(abe,mona).
marriedBasic(clancy,jackie).
marriedBasic(homer,marge).
married(X,Y) :- marriedBasic(X,Y).
married(X,Y) :- marriedBasic(Y,X).


% Transitive definition that doesn't loop on failure.
ltByOne(one,two).
ltByOne(two,three).
ltByOne(three,four).
lt(X,Y) :- ltByOne(X,Y).
lt(X,Z) :- ltByOne(X,Y), lt(Y,Z).
