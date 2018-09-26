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
mother(valeria, oksana).
mother(tom, ksenia).

married(kelly, martin).
married(piret, mart).
married(eevi, david).
married(kersti, toomas).
married(veronica, ago).
married(valeria, tom).
married(oksana, jack).
married(ksenia, joshua).

male(erki).
male(neeme).
male(arnold).
male(martin).
male(mart).
male(david).
male(toomas).
male(ago).
male(tom).
male(jack).
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
female(ksenia).

father(Child, Father):- male(Father), married(Mother, Father), mother(Child, Mother).
brother(Child, Brother):- male(Brother), mother(Child, Mother), mother(Brother, Mother), not(Child = Brother).
sister(Child, Sister):- female(Sister), mother(Child, Mother), mother(Sister, Mother), not(Child = Sister).
aunt(Child, Aunt):- female(Aunt), (sister(Father, Aunt), father(Child, Father)); (sister(Mother, Aunt), mother(Child, Mother)).
uncle(Child, Uncle):- male(Uncle), (brother(Father, Uncle), father(Child, Father)); (brother(Mother, Uncle), mother(Child, Mother)).
grandmother(Child, Grandmother):- female(Grandmother), (mother(Child, Mother), mother(Mother, Grandmother)); (father(Child, Father), mother(Father, Grandmother)).
grandfather(Child, Grandfather):- male(Grandfather), grandmother(Child, Grandmother), married(Grandmother, Grandfather).

ancestor(Child, Parent):- mother(Child, Parent); father(Child, Parent).
ancestor(Child, Parent):- (mother(Child, Mother), ancestor(Mother, Parent)); (father(Child, Father), ancestor(Father, Parent)).

female_ancestor(Child, Parent):- mother(Child, Parent).
female_ancestor(Child, Parent):- mother(Child, Mother), female_ancestor(Mother, Parent).

male_ancestor(Child, Parent):- father(Child, Parent).
male_ancestor(Child, Parent):- father(Child, Father), male_ancestor(Father, Parent).

