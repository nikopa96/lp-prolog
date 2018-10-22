member(X, [X|_]).
member(X, [_|Tail]):-
    member(X, Tail).

yhisosa([], _, []).
yhisosa([El|Tail], SecondList, [El|NewList]):-
    member(El, SecondList), yhisosa(Tail, SecondList, NewList).
yhisosa([El|Tail], SecondList, NewList):-
    not(member(El, SecondList)), yhisosa(Tail, SecondList, NewList).

yhend([], El, El).
yhend([El|Tail], SecondList, NewList):-
    member(El, SecondList), !, yhend(SecondList, Tail, NewList).
yhend([El|Tail], SecondList, [El|NewList]):-
    yhend(SecondList, Tail, NewList).

vahe([], _, []).
vahe([El|Tail], SecondList, [El|NewList]):-
    not(member(El, SecondList)), vahe(Tail, SecondList, NewList).
vahe([El|Tail], SecondList, NewList):-
    member(El, SecondList), vahe(Tail, SecondList, NewList).

ristkorrutis_alam([], _, []).
ristkorrutis_alam([El1|Tail1], [El2|Tail2], [[El1, El2]|NewList]):-
    ristkorrutis_alam(Tail1, [El2|Tail2], NewList).

ristkorrutis(_, [], []).
ristkorrutis(FirstList, [El|Tail], NewList):-
    ristkorrutis_alam(FirstList, [El], Cort), append(Cort, TempList, NewList), ristkorrutis(FirstList, Tail, TempList).