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

father(Child, Father):- male(Father), married(Mother, Father), mother(Child, Mother), female(Mother).
