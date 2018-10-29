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

eats(kapsauss, kapsas).
eats(loom, selgroogne).
eats(loom, selgrootu).
eats(selgroogne, selgrootu).
eats(selgrootu, oistaim).
eats(selgrootu, puuk).
eats(loom, taim).

is_terminal(Parent, Node):-
    on_eriliik(Node, Parent),
    not(on_eriliik(_, Node)).

find_terminals(Node, NextNode):-
    on_eriliik(NextNode, Node), is_terminal(Node, NextNode).
find_terminals(Node, NextNode):-
    on_eriliik(MiddleNode, Node),
    find_terminals(MiddleNode, NextNode).

count_terminals_alam(Node, NewList):-
    findall(TerminalNode, find_terminals(Node, TerminalNode), NewList).

count_terminals(Node, [Node], 1):-
    not(find_terminals(Node, _)), !.
count_terminals(Node, Terminals, Count):-
    count_terminals_alam(Node, Terminals), length(Terminals, Count).