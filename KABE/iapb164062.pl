:- module('Nikolai Kopa', [iapb164062/3]).

iapb164062(MyColor, Xin, Yin):-
    Xin = 0, Yin = 0,
    optimaalne_lahendus(MyColor), !.
iapb164062(MyColor, Xin, Yin):-
    (Xin =\= 0, Yin =\= 0), X is Xin, Y is Yin,
    tammid(MyColor, TammColor),
    aktiivsed_nupud(TammColor, X, Y),
    tamm_kaigu_variandid(X, Y), !.
iapb164062(MyColor, Xin, Yin):-
    (Xin =\= 0, Yin =\= 0), X is Xin, Y is Yin, ruut(X, Y, MyColor),
    leia_suund(MyColor, Suund),
    kaigu_variandid(X, Y, Suund, _, _), !.
iapb164062(_, _, _).

tamm(10).
tamm(20).

leia_suund(1,1):- !.
leia_suund(2,-1).

%--------------------------------
kaigu_variandid(X,Y,Suund,X1,Y1):-
    votmine(X,Y,Suund,X1,Y1),!.
kaigu_variandid(X,Y,Suund,X1,Y1):-
    kaimine(X,Y,Suund,X1,Y1),!.
%--------------------------------
votmine(X,Y,Suund,X1,Y1):-
    not(ruut(X, Y, 10)),
    not(ruut(X, Y, 20)),
    kas_saab_votta(X,Y,Suund,X1,Y1,X2,Y2),
    vota(X,Y,Suund,X1,Y1,X2,Y2),
    fail.

vota(X, Y, _, X1, Y1, X2, Y2):-
    ruut(X, Y, Status),
    tammi_moodustamine(Status, NewStatus, X2),
    retract(ruut(X, Y, _)),
    retract(ruut(X1, Y1, _)),
    retract(ruut(X2, Y2, _)),
    assert(ruut(X, Y, 0)),
    assert(ruut(X1, Y1, 0)),
    assert(ruut(X2, Y2, NewStatus)).

%--------
kas_saab_votta(X,Y,Suund,X1,Y1,X2,Y2):-  % Votmine edasi paremale
    X1 is X + Suund,
    Y1 is Y + 1,
    ruut(X, Y, MyColor),
    ruut(X1,Y1, Color),
    Color =\= MyColor, Color =\= 0,
    X2 is X1 + Suund,
    Y2 is Y1 + 1,
    ruut(X2,Y2, 0).
kas_saab_votta(X,Y,Suund,X1,Y1,X2,Y2):-  % Votmine edasi vasakule
    X1 is X + Suund,
    Y1 is Y - 1,
    ruut(X, Y, MyColor),
    ruut(X1,Y1, Color),
    Color =\= MyColor, Color =\= 0,
    X2 is X1 + Suund,
    Y2 is Y1 - 1,
    ruut(X2,Y2, 0).
kas_saab_votta(X,Y,Suund,X1,Y1,X2,Y2):-  % Votmine tagasi paremale
    X1 is X + Suund * -1,
    Y1 is Y + 1,
    ruut(X, Y, MyColor),
    ruut(X1,Y1, Color),
    Color =\= MyColor, Color =\= 0,
    X2 is X1 + Suund * -1,
    Y2 is Y1 + 1,
    ruut(X2,Y2, 0).
kas_saab_votta(X,Y,Suund,X1,Y1,X2,Y2):-  % Votmine tagasi vasakule
    X1 is X + Suund * -1,
    Y1 is Y - 1,
    ruut(X, Y, MyColor),
    ruut(X1,Y1, Color),
    Color =\= MyColor, Color =\= 0,
    X2 is X1 + Suund * -1,
    Y2 is Y1 - 1,
    ruut(X2,Y2, 0).

%--------------------------------
kaimine(X,Y,Suund,X1,Y1):-
    kas_naaber_vaba(X,Y,Suund,X1,Y1),
    tee_kaik(X,Y,X1,Y1),
    write([' kaib ', X1,Y1]),
    fail.
kaimine(_,_,_,_,_).

kas_naaber_vaba(X,Y,Suund,X1,Y1):-
    X1 is X +Suund,
    Y1 is Y + 1,
    ruut(X1,Y1, 0).
kas_naaber_vaba(X,Y,Suund,X1,Y1):-
    X1 is X +Suund,
    Y1 is Y -1,
    ruut(X1,Y1, 0), write(' voi ').

kas_naaber_vaba(X,Y,X1,Y1):-
    ruut(X,Y, Status),
    assert(ruut1(X1,Y1, Status)),!.

kas_naaber_vaba_tamm(X, Y, X1, Y1):-
    kas_naaber_vaba(X, Y, 1, X1, Y1);
    kas_naaber_vaba(X, Y, -1, X1, Y1).

tee_kaik(X, Y, X1, Y1):-
    ruut(X, Y, Status1),
    ruut(X1, Y1, Status2),
    tammi_moodustamine(Status1, NewStatus, X1),
    retract(ruut(X, Y, Status1)),
    retract(ruut(X1, Y1, Status2)),
    assert(ruut(X, Y, Status2)),
    assert(ruut(X1, Y1, NewStatus)).

%---------------TAMM----------------
tammi_moodustamine(1, 10, 8):- !.
tammi_moodustamine(2, 20, 1):- !.
tammi_moodustamine(Color, Color, _).

kas_tamm_saab_votta(MyColor, X, Y, Xend, Yend, Xafter, Yafter):-
    leida_diagonaal_alam_1(MyColor, X, Y, 1, Xend, Yend, Xafter, Yafter);
    leida_diagonaal_alam_2(MyColor, X, Y, 1, Xend, Yend, Xafter, Yafter);
    leida_diagonaal_alam_1(MyColor, X, Y, -1, Xend, Yend, Xafter, Yafter);
    leida_diagonaal_alam_2(MyColor, X, Y, -1, Xend, Yend, Xafter, Yafter).

leida_diagonaal_alam_1(MyColor, X, Y, Suund, Xend, Yend, Xafter, Yafter):-
    Xend is X + Suund,
    Yend is Y + 1,
    Xafter is Xend + Suund,
    Yafter is Yend + 1,
    ((MyColor = 10, (ruut(Xend, Yend, 2); ruut(Xend, Yend, 20)), ruut(Xafter, Yafter, 0));
    (MyColor = 20, (ruut(Xend, Yend, 1); ruut(Xend, Yend, 10)), ruut(Xafter, Yafter, 0))).
leida_diagonaal_alam_1(MyColor, X, Y, Suund, Xend, Yend, Xafter, Yafter):-
    X1 is X + Suund,
    Y1 is Y + 1,
    ruut(X1, Y1, 0),
    leida_diagonaal_alam_1(MyColor, X1, Y1, Suund, Xend, Yend, Xafter, Yafter).

leida_diagonaal_alam_2(MyColor, X, Y, Suund, Xend, Yend, Xafter, Yafter):-
    Xend is X + Suund,
    Yend is Y - 1,
    Xafter is Xend + Suund,
    Yafter is Yend - 1,
    ((MyColor = 10, (ruut(Xend, Yend, 2); ruut(Xend, Yend, 20)), ruut(Xafter, Yafter, 0));
    (MyColor = 20, (ruut(Xend, Yend, 1); ruut(Xend, Yend, 10)), ruut(Xafter, Yafter, 0))).
leida_diagonaal_alam_2(MyColor, X, Y, Suund, Xend, Yend, Xafter, Yafter):-
    X1 is X + Suund,
    Y1 is Y - 1,
    ruut(X1, Y1, 0),
    leida_diagonaal_alam_2(MyColor, X1, Y1, Suund, Xend, Yend, Xafter, Yafter).

tamm_kaigu_variandid(X, Y):-
    ruut(X, Y, Color),
    kas_tamm_saab_votta(Color, X, Y, X1, Y1, X2, Y2),
    vota(X, Y, _, X1, Y1, X2, Y2), !.
tamm_kaigu_variandid(X, Y):-
    (kas_naaber_vaba(X, Y, 1, X1, Y1); kas_naaber_vaba(X, Y, -1, X1, Y1)),
    tee_kaik(X, Y, X1, Y1), !.

%----------TEHISINTELLEKT-----------

nupp(1, 10).
nupp(1, 1).
nupp(2, 20).
nupp(2, 2).

tammid(1, 10).
tammid(2, 20).

aktiivsed_nupud(Color, X, Y):-
    (Color =:= 10; Color =:= 20),
    ruut(X, Y, Color),
    (kas_naaber_vaba_tamm(X, Y, _, _);
    kas_tamm_saab_votta(Color, X, Y, _, _, _, _)), !.
aktiivsed_nupud(Color, X, Y):-
    Color =\= 10, Color =\= 20,
    ruut(X, Y, Color),
    leia_suund(Color, Suund),
    (kas_naaber_vaba(X, Y, Suund, _, _);
    kas_saab_votta(X, Y, Suund, _, _, _, _)).

optimaalne_lahendus(MyColor):-
    tammid(MyColor, TammColor),
    aktiivsed_nupud(TammColor, X, Y),
    kas_tamm_saab_votta(TammColor, X, Y, X1, Y1, X2, Y2),
    vota(X, Y, _, X1, Y1, X2, Y2), !.
optimaalne_lahendus(MyColor):-
    aktiivsed_nupud(MyColor, X, Y),
    leia_suund(MyColor, Suund),
    kas_saab_votta(X, Y, Suund, X1, Y1, X2, Y2),
    vota(X, Y, Suund, X1, Y1, X2, Y2), !.
optimaalne_lahendus(MyColor):-
    tammid(MyColor, TammColor),
    aktiivsed_nupud(TammColor, X, Y),
    (kas_naaber_vaba(X, Y, 1, X1, Y1); kas_naaber_vaba(X, Y, -1, X1, Y1)),
    tee_kaik(X, Y, X1, Y1), !.
optimaalne_lahendus(MyColor):-
    aktiivsed_nupud(MyColor, X, Y),
    leia_suund(MyColor, Suund),
    kas_naaber_vaba(X, Y, Suund, X1, Y1),
    tee_kaik(X, Y, X1, Y1), !.