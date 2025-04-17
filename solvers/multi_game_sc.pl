/*
	Game description language top-level 
	game independent interpreter - acts 
        like an FSM.

	author: Kostas Stathis
	created: 7/12/23
	last update: 9/04/25
*/

/*
:- dynamic initially/2.
:- dynamic initial/1.

% All legal evolutions of a game: can be used both as a generator and test.
game(F,F):- final(F).  
game(S,F):- \+ final(S), legal(M,S), game(do(M,S),F).

% Situation Calculus - our formulation for games.
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).
*/

:- dynamic demo/3.
:- multifile demo/3.
:- multifile pd/2.

demo(T, Class, game(F,F)):- 
	demo(T, Class, final(F)).  

demo(T, Class, game(S,F)):- 
	demo(T, Class, \+ final(S)), 
	demo(T, Class, legal(M,S)), 
	demo(T, Class, game(do(M,S),F)).

demo(T, Class, holds(F, T)):- 
	demo(T, Class, initially(F)).
demo(T, Class, holds(F, do(M, S))):- 
	demo(T, Class, effect(F, do(M, S), S)).
demo(T, Class, holds(F, do(A, S))):- 
	demo(T, Class, holds(F, S)), 
	demo(T, Class, \+ abnormal(F, A, S)).

demo_conj(_, _, []):- !.
demo_conj(T, Class, [Head|Tail]):-
	demo(T, Class, Head),
	demo_conj(T, Class, Tail).

demo(T, Class, \+ Goal):-
	!,
	\+ demo(T, Class, Goal).
demo(_,_,Goal):-
	system(Goal),
	!,
	call(Goal).
demo(T, Class, Goal):-
	ClassRule =..[Class, Goal, Body],
	call(ClassRule),
	demo_conj(T, Class, Body).



