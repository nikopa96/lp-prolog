max3(X, Y, X) :- X > Y, !.
max3(X, Y, Y) :- X =< Y.

on_eriliik(s,d).
on_eriliik(t,d).
on_eriliik(s1,s).
on_eriliik(s2,s).
on_eriliik(s11,s1).
on_eriliik(s12,s1).
on_eriliik(s21,s2).
on_eriliik(s22,s2).
on_eriliik(t1,t).
on_eriliik(t2,t).
on_eriliik(t21,t2).
on_eriliik(t22,t2).

eats(t1,t2).
eats(s2,t1).
eats(s1,s2).

is_terminal(Parent, Node):-
    on_eriliik(Node, Parent),
    not(on_eriliik(_, Node)).

find_terminals(Node, NextNode):-
    on_eriliik(NextNode, Node), is_terminal(Node, NextNode).
find_terminals(Node, NextNode):-
    on_eriliik(MiddleNode, Node),
    find_terminals(MiddleNode, NextNode).

count_terminals_alam(Node, NewList):-
    find_terminals(Node, _), findall(TerminalNode, find_terminals(Node, TerminalNode), NewList), !.
count_terminals_alam(Node, [Node]):-
    not(find_terminals(Node, _)).

count_terminals(Node, Terminals, Count):-
    count_terminals_alam(Node, Terminals), length(Terminals, Count).

find_extinction(Node, PreviousNode):-
    eats(PreviousNode, Node).
find_extinction(Node, PreviousNode):-
    eats(MiddleNode, Node),
    find_extinction(MiddleNode, PreviousNode).

extinction_alam_1(Node, [Node|NewList]):-
    findall(Extinction, find_extinction(Node, Extinction), NewList).

extinction_alam_2([], []).
extinction_alam_2([El|List], ExtinctionList):-
    count_terminals_alam(El, NewList), append(NewList, TempList, ExtinctionList), extinction_alam_2(List, TempList). 

extinction(Who, What_spieces, How_many):-
    extinction_alam_1(Who, List),
    extinction_alam_2(List, What_spieces),
    length(What_spieces, How_many).

:-dynamic extinction_stat/3.

extinction_define_facts([], _, Extinction):-
    extinction(Extinction, What_spieces, How_many),
    assert(extinction_stat(Extinction, What_spieces, How_many)).
extinction_define_facts([El|List], Max, Extinction):-
    extinction(El, _, How_many),
    ((How_many =< Max, extinction_define_facts(List, Max, Extinction));
    (How_many > Max, extinction_define_facts(List, How_many, El))), !.

find_most_sensitive_species(Node, N, List):-
    extinction_alam_1(Node, [PreLastNode|ExtinctionList]),
    eats(LastNode, PreLastNode),
    extinction_define_facts([LastNode, PreLastNode, ExtinctionList], 0, NewNode),
    extinction_stat(NewNode, N, List), 
    retractall(extinction_stat(_, _, _)), !.