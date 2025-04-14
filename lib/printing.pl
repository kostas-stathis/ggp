writelist([]).
writelist([H|T]):-
        write(H),!,
        writelist(T).

writelistnl(L):-
        writelist(L),
        nl.
