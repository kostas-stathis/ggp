
/* Reasoning predicates
 --------------------------------------------
*/

nash(M1:U1, M2:U2):-
	game(rps(p,o, M1, M2, U1, U2)),
	U1 >= U2,
	U2 >= U1.

choices(rps(P, O, Mp, Mo, Up, Uo), Choices):-
	findall(Up-Mp, game(rps(P, O, Mp, Mo, Up, Uo)), L),
	keysort(L, NL),
	reverse(NL, Choices).

% Assuming opponent plays Mo, which move Mp should I play with 
% at least as good utility as his?

best_response_to(Mo,Mp):-
	(ground(Mo) ->
                (var(Mp) ->
                        game(rps(p,o,Mp,Mo,Up,Uo)),
                        Up>=Uo
                        ;
                        write('Second arg must be a var.'),
                        nl
                )
                ;
                write('First arg must be ground.'),
                nl
        ).

/* To delete
best_if_opp_does(Mo,Mp:Up):-
	%(ground(Mo) ->
		%(var(Mp:Up) ->
		 	game(rps(p,o,Mp,Mo,Up,Uo)). 
			Up>=Uo
			%;
			%write('Second arg must be a var.'),
			%nl
		%)
		%;
		%write('First arg must be ground.'),
		%nl
	%).
*/
