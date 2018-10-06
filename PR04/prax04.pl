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


is_a('Juhid', 'Ametid').

is_a('Seadusandjad, korgemad ametnikud ja juhid', 'Juhid').
is_a('Seadusandjad ja korgemad ametnikud', 'Seadusandjad, korgemad ametnikud ja juhid').
is_a('Seadusandjad', 'Seadusandjad ja korgemad ametnikud').
is_a('Korgemad valitsusametnikud', 'Seadusandjad ja korgemad ametnikud').
is_a('Aleviku- ja kulavanemad', 'Seadusandjad ja korgemad ametnikud').
is_a('Seadusandja', 'Seadusandjad').
is_a('Korgem valitsusametnik', 'Korgemad valitsusametnikud').
is_a('Aleviku- ja kulavanem', 'Aleviku- ja kulavanemad').

is_a('Juhid tegevusalade jargi', 'Juhid').
is_a('Juhid toostuses, kaevanduses, ehituses ja turustamises', 'Juhid tegevusalade jargi').
is_a('Juhid toostuses', 'Juhid toostuses, kaevanduses, ehituses ja turustamises').
is_a('Juhid kaevanduses', 'Juhid toostuses, kaevanduses, ehituses ja turustamises').
is_a('Juht toostuses', 'Juhid toostuses').
is_a('Juht kaevanduses', 'Juhid kaevanduses').

is_a('Tehnikud ja keskastme spetsialistid', 'Ametid').

is_a('Tervishoiu keskastme spetsialistid', 'Tehnikud ja keskastme spetsialistid').
is_a('Meditsiinitehnikud ja abiapteekrid', 'Tervishoiu keskastme spetsialistid').
is_a('Meditsiiniliste kuvamis- ja raviseadmete tehnikud', 'Meditsiinitehnikud ja abiapteekrid').
is_a('Abiapteekrid', 'Meditsiinitehnikud ja abiapteekrid').
is_a('Meditsiiniliste ja hambaproteeside tehnikud', 'Meditsiinitehnikud ja abiapteekrid').
is_a('Meditsiiniliste kuvamis- ja raviseadmete tehnik', 'Meditsiiniliste kuvamis- ja raviseadmete tehnikud').
is_a('Abiapteeker', 'Abiapteekrid').
is_a('Meditsiiniliste ja hambaproteeside tehnik', 'Meditsiiniliste ja hambaproteeside tehnikud').

is_a('Info- ja kommunikatsioonitehnoloogia tehnilised tootajad', 'Tehnikud ja keskastme spetsialistid').
is_a('Telekommunikatsiooni ja ringhaalingu tehnikud', 'Info- ja kommunikatsioonitehnoloogia tehnilised tootajad').
is_a('Ringhaalingu ja audiovisuaalala tehnikud', 'Telekommunikatsiooni ja ringhaalingu tehnikud').
is_a('Telekommunikatsiooni tehnikud', 'Telekommunikatsiooni ja ringhaalingu tehnikud').
is_a('Ringhaalingu ja audiovisuaalala tehnik', 'Ringhaalingu ja audiovisuaalala tehnikud').
is_a('Telekommunikatsiooni tehnik', 'Telekommunikatsiooni tehnikud').


is_a(evelin, 'Abiapteeker').
is_a(erki, 'Juht toostuses').
is_a(valeri, 'Seadusandja').
is_a(anne, 'Korgem valitsusametnik').
is_a(neeme, 'Aleviku- ja kulavanem').
is_a(caroline, 'Telekommunikatsiooni tehnik').
is_a(arnold, 'Meditsiiniliste ja hambaproteeside tehnik').

is_a(kelly, 'Seadusandja').
is_a(martin, 'Korgem valitsusametnik').
is_a(piret, 'Ringhaalingu ja audiovisuaalala tehnik').
is_a(mart, 'Juht kaevanduses').
is_a(eevi, 'Meditsiiniliste ja hambaproteeside tehnik').
is_a(david, 'Juht toostuses').

is_a(kersti, 'Aleviku- ja kulavanem').
is_a(toomas, 'Juht kaevanduses').
is_a(veronica, 'Abiapteeker').
is_a(ago, 'Korgem valitsusametnik').
is_a(ott, 'Abiapteeker').
is_a(mihkel, 'Juht toostuses').

is_a(valeria, 'Seadusandja').
is_a(tom, 'Abiapteeker').
is_a(oksana, 'Juht toostuses').
is_a(jack, 'Meditsiiniliste ja hambaproteeside tehnik').
is_a(kaja, 'Abiapteeker').
is_a(andrus, 'Juht toostuses').
is_a(ksenia, 'Korgem valitsusametnik').
is_a(joshua, 'Seadusandja').


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

occupation(Who, Relative, O):-
    (Relative = mother, mother(Who, Mother), is_a(Mother, O));
    (Relative = father, father(Who, Father), is_a(Father, O));
    (Relative = brother, brother(Who, Brother), is_a(Brother, O));
    (Relative = sister, sister(Who, Sister), is_a(Sister, O));
    (Relative = aunt, aunt(Who, Aunt), is_a(Aunt, O));
    (Relative = uncle, uncle(Who, Uncle), is_a(Uncle, O));
    (Relative = grandmother, grandmother(Who, Grandmother), is_a(Grandmother, O));
    (Relative = grandfather, grandfather(Who, Grandfather), is_a(Grandfather, O)).

who_is(O, Who):-
    is_a(Who, O), (male(Who); female(Who)).
who_is(O, Who):- 
    is_a(MiddleSpeciality, O), 
    who_is(MiddleSpeciality, Who).