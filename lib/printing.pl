writelist([]).
writelist([H|T]):-
        write(H),!,
        writelist(T).

writelistnl(L):-
        writelist(L),
        nl.

system(Goal):-
        predicate_property(system:Goal, built_in).
