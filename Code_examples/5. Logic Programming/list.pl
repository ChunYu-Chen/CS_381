% Some simple list "functions".

% elem/2 - Is the element a member of the list?
% elem(_,[]) :- fail.
elem(X, [X|_]).
elem(X, [_|T]) :- elem(X,T).

% lengthInt/2 - Length of a list.
lengthInt([], 0).
lengthInt([_|T], L) :- lengthInt(T,LengthOfT), L is LengthOfT + 1.

% sum/2 - Sum of a list of integers.
sum([], 0).
sum([H|T], S) :- sum(T,SumOfT), S is SumOfT + H.

% concat/3 - Concatenate two lists.
concat([], L, L).
concat([H|L1], L2, [H|L3]) :- concat(L1, L2, L3).

% ?- concat([1,2], [3], Result).
%    H = 1, L1 = [2], L2 = [3], Result = [H|L3], L3 = [2,3]
%    ?- concat([2], [3], L3).
%       H = 2, L1 = [], L2 = [3], L3 = [H|L3_2], L3_2 = [3]
%       ?- concat([], [3], L3_2).
%          L = [3], L3_2 = [3].
