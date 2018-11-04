on_eriliik(loom, elusloend).
on_eriliik(taim, elusloend).

on_eriliik(selgroogne, loom).
on_eriliik(lehm, selgroogne).

on_eriliik(selgrootu, loom).
on_eriliik(puuk, selgrootu).
on_eriliik(kapsauss, selgrootu).

on_eriliik(oistaim, taim).

on_eriliik(mitteoistaim, taim).
on_eriliik(kapsas, mitteoistaim).

eats(oistaim, mitteoistaim).
eats(selgrootu, oistaim).
eats(selgroogne, selgrootu).

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

find_extinction(Node, NextNode):-
    eats(Node, NextNode).
find_extinction(Node, NextNode):-
    eats(Node, MiddleNode), 
    find_extinction(MiddleNode, NextNode).

extinction_alam_1(Node, NewList):-
    findall(Extinction, find_extinction(Node, Extinction), NewList).

extinction_alam_2([], []).
extinction_alam_2([El|List], ExtinctionList):-
    count_terminals_alam(El, NewList), append(NewList, TempList, ExtinctionList), extinction_alam_2(List, TempList). 

extinction(Who, What_spieces, How_many):-
    extinction_alam_1(Who, List),
    extinction_alam_2(List, What_spieces),
    length(What_spieces, How_many).
