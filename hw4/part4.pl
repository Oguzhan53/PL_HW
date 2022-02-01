element(X,[X|_]).
element(X,[_|A]):-element(X,A).


eq([],_).
eq([H|T],Z):-
    element(H,Z),
    eq(T,Z).

equivalent(X,Y):-
    eq(X,Y),
    eq(Y,X).




