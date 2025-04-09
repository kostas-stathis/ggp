/*
	Game description language top-level 
	game independent interpreter - acts 
        like an FSM.

	author: Kostas Stathis
	created: 7/12/23
	last update: 9/04/25
*/

% All valid games of any game
game(F,F):- final(F).  
game(S,F):- \+ final(S), valid(M,S), game(do(M,S),F).

% A move is valid if it is legal
valid(M, S):- legal(M, S).

% State evolution in the SC - to be tested, works for pd
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).
