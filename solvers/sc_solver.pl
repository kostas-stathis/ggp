/*
	Game description language top-level 
	game independent interpreter - acts 
        like an FSM.

	author: Kostas Stathis
	created: 7/12/23
	last update: 9/04/25
*/

:- dynamic initially/2.
:- dynamic initial/1.

% All legal evolutions of a game: can be used both as a generator and test.
game(F,F):- final(F).  
game(S,F):- \+ final(S), legal(M,S), game(do(M,S),F).

% Situation Calculus - our formulation for games.
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).
