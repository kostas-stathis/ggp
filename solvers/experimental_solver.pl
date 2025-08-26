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
% [Id]
game(F):- final(F).  
game(S):- \+ final(S), legal(M,S), game([M|S]).

% Situation Calculus - our formulation for games.
holds(F, [Id]):- initially(F, Id).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).


/*

holds(F, Id:S):- initially(F, Id:S).
holds(F, Id:do(M, S)):- effect(F, M, Id:S).
holds(F, Id:do(A, S)):- holds(F, Id:S), \+ abnormal(F, A, Id:S).

*/
