append([], A, A).
append([A|B], C, [A|D]):-
    append(B, C, D).

viimane_element(X, [X]).
viimane_element(X, [_|Tail]):-
    viimane_element(X, Tail).

suurim([], []).
suurim([El], [El]).
suurim([El1, El2|Tail], NewList):-
    (El1 >= El2, suurim([El2|Tail], TempList), append([El1], TempList, NewList));
    (El1 < El2, suurim([El2|Tail], TempList), append([El2], TempList, NewList)).

paki([], []).
paki([El], [El]).
paki([El, El|Tail], NewList):-
    paki([El|Tail], NewList), !.
paki([El1, El2|Tail1], [El1|Tail2]):-
    paki([El2|Tail1], Tail2).

duplikeeri([], []).
duplikeeri([El1|Tail], NewList):-
    duplikeeri(Tail, TempList),
    append([El1, El1], TempList, NewList).

kordista([], _, []).
kordista([El], _, [El]).

paaris_arv(X):-
    0 =:= mod(X, 2).
paaritu_arv(X):-
    1 =:= mod(X, 2).

vordle_predikaadiga([], [paaris_arv], []).
vordle_predikaadiga([El|Tail], [Predicate], NewList):-
    Predicate = paaris_arv,
        ((paaris_arv(El), append([El], TempList, NewList), vordle_predikaadiga(Tail, [Predicate], TempList));
        (paaritu_arv(El), vordle_predikaadiga(Tail, [Predicate], NewList))), !.

vordle_predikaadiga([], [paaritu_arv], []).
vordle_predikaadiga([El|Tail], [Predicate], NewList):-
    Predicate = paaritu_arv,
        ((paaritu_arv(El), append([El], TempList, NewList), vordle_predikaadiga(Tail, [Predicate], TempList));
        (paaris_arv(El), vordle_predikaadiga(Tail, [Predicate], NewList))), !.

vordle_predikaadiga([], [suurem_kui, _], []).
vordle_predikaadiga([El|Tail], [Predicate, Param], NewList):-
    Predicate = suurem_kui,
        ((El > Param, append([El], TempList, NewList), vordle_predikaadiga(Tail, [Predicate, Param], TempList));
        (El =< Param, vordle_predikaadiga(Tail, [Predicate, Param], NewList))), !.
        