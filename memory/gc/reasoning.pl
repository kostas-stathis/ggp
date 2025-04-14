
/* Reasoning predicates
 --------------------------------------------
*/

/* 
  On the assumption of that j plays sj, 
  i has no other option than to play si,
  and vice versa. 
*/
nash(M1:U1, M2:U2):-
	game(gc(p,o, M1, M2, U1, U2)),
	U1 >= U2,
	U2 >= U1.

choices(gc(P, O, Mp, Mo, Up, Uo), Choices):-
	findall(Up-Mp, game(gc(P, O, Mp, Mo, Up, Uo)), L),
	keysort(L, NL),
	reverse(NL, Choices).

% Assuming opponent plays Mo, which move Mp should I play with
% at least as good utility as his?

best_response_to(Mo:Uo,Mp:Up):-
        (is_ground_mutil(Mo:Uo) ->
                (is_var_mutil(Mp:Up) ->
                        game(gc(p,o,Mp,Mo,Up,Uo)),
                        Up>=Uo
                        ;
                        write('Second arg must be a var.'),
                        nl
                )
                ;
                write('First arg must be ground.'),
                nl
        ).


is_var_mutil(M1:U1):-
        (var(M1) -> 
                (var(U1) -> true; write('Utility is not var.\n'))
                ;
                write('Move is not var.\n')
        ).

is_ground_mutil(M1:U1):-
        (ground(M1) -> 
                (ground(U1) -> true; write('Utility is not ground.\n'))
                ;
                write('Move is not ground.\n')
        ).
