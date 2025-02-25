mother(evelin, kelly).
mother(erki, kelly).
mother(valeri, kelly).
mother(anne, piret).
mother(neeme, piret).
mother(caroline, eevi).
mother(arnold, eevi).
mother(martin, kersti).
mother(piret, kersti).
mother(mart, veronica).
mother(eevi, veronica).
mother(kersti, valeria).
mother(toomas, oksana).
mother(veronica, kaja).
mother(ago, ksenia).
mother(ott, ksenia).
mother(mihkel, ksenia).

married(kelly, martin).
married(piret, mart).
married(eevi, david).
married(kersti, toomas).
married(veronica, ago).
married(valeria, tom).
married(oksana, jack).
married(kaja, andrus).
married(ksenia, joshua).

male(erki).
male(valeri).
male(neeme).
male(arnold).
male(martin).
male(mart).
male(david).
male(toomas).
male(ago).
male(ott).
male(mihkel).
male(tom).
male(jack).
male(andrus).
male(joshua).

female(evelin).
female(anne).
female(caroline).
female(kelly).
female(piret).
female(eevi).
female(kersti).
female(veronica).
female(valeria).
female(oksana).
female(kaja).
female(ksenia).

father(Child, Father):-
    male(Father), married(Mother, Father),
    mother(Child, Mother).

brother(Child, Brother):-
    male(Brother),
    mother(Child, Mother),
    mother(Brother, Mother),
    not(Child = Brother).

sister(Child, Sister):-
    female(Sister),
    mother(Child, Mother),
    mother(Sister, Mother),
    not(Child = Sister).

aunt(Child, Aunt):-
    female(Aunt),
    (sister(Father, Aunt), father(Child, Father));
    (sister(Mother, Aunt), mother(Child, Mother)).

uncle(Child, Uncle):-
    male(Uncle),
    (brother(Father, Uncle), father(Child, Father));
    (brother(Mother, Uncle), mother(Child, Mother)).

grandmother(Child, Grandmother):-
    female(Grandmother),
    (mother(Child, Mother), mother(Mother, Grandmother));
    (father(Child, Father), mother(Father, Grandmother)).

grandfather(Child, Grandfather):-
    male(Grandfather),
    grandmother(Child, Grandmother),
    married(Grandmother, Grandfather).

ancestor(Child, Parent):-
    mother(Child, Parent);
    father(Child, Parent).
ancestor(Child, Parent):-
    (mother(Child, Mother), ancestor(Mother, Parent));
    (father(Child, Father), ancestor(Father, Parent)).

female_ancestor(Child, Parent):-
    mother(Child, Parent).
female_ancestor(Child, Parent):-
    (mother(Child, Mother), female_ancestor(Mother, Parent));
    (father(Child, Father), female_ancestor(Father, Parent)).

male_ancestor(Child, Parent):-
    father(Child, Parent).
male_ancestor(Child, Parent):-
    (mother(Child, Mother), male_ancestor(Mother, Parent));
    (father(Child, Father), male_ancestor(Father, Parent)).

ancestor1(Child, Parent, N):- N =:= 1,
    (mother(Child, Parent); father(Child, Parent)).
ancestor1(Child, Parent, N):- N > 1,
    ((mother(Child, Mother), ancestor1(Mother, Parent, N - 1));
    (father(Child, Father), ancestor1(Father, Parent, N - 1))).

ancestor2(Child, Parent, X):-
    (mother(Child, Parent); father(Child, Parent)),
    aggregate_all(count, (mother(_, Parent);(father(_, Parent))), NumberOfChildren),
    NumberOfChildren > X.
ancestor2(Child, Parent, X):-
    (mother(Child, Mother), ancestor2(Mother, Parent, X));
    (father(Child, Father), ancestor2(Father, Parent, X)).
