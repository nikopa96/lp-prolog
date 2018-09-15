mother(evelin, kelly).
mother(erki, kelly).
mother(anne, piret).
mother(neeme, piret).
mother(caroline, eevi).
mother(arnold, eevi).
mother(martin, kersti).
mother(piret, kersti).
mother(mart, veronica).
mother(eevi, veronica).
mother(toomas, valeria).
mother(veronica, valeria).

married(kelly, martin).
married(piret, mart).
married(eevi, david).
married(kersti, toomas).
married(veronica, ago).
married(valeria, tom).

male(erki).
male(neeme).
male(arnold).
male(martin).
male(mart).
male(david).
male(toomas).
male(ago).
male(tom).

female(evelin).
female(anne).
female(caroline).
female(kelly).
female(piret).
female(eevi).
female(kersti).
female(veronica).
female(valeria).

father(Child, Father):- male(Father), married(Mother, Father), mother(Child, Mother).
brother(Child, Brother):- male(Brother), mother(Child, Mother), mother(Brother, Mother), father(Child, Father), father(Brother, Father).
sister(Child, Sister):- female(Sister), mother(Child, Mother), mother(Sister, Mother), father(Child, Father), father(Sister, Father).
aunt(Child, Aunt):- female(Aunt), (sister(Father, Aunt), father(Child, Father)); (sister(Mother, Aunt), mother(Child, Mother)).
uncle(Child, Uncle):- male(Uncle), (brother(Father, Uncle), father(Child, Father)); (brother(Mother, Uncle), mother(Child, Mother)).
grandmother(Child, Grandmother):- female(Grandmother), (mother(Child, Mother), mother(Mother, Grandmother)); (father(Child, Father), mother(Father, Grandmother)).
grandfather(Child, Grandfather):- male(Grandfather), grandmother(Child, Grandmother), married(Grandmother, Grandfather).
