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

:- discontiguous demo/2.
:- dynamic rule/3.
:- multifile rule/3.
:- dynamic instance_of/2.
:- multifile instance_of/2.


demo_conj(_, []):- !.
demo_conj(T, [Goal|Goals]):-
	demo(T, Goal),
	demo_conj(T, Goals).

demo(T, game(F,F)):- 
	demo(T, final(F)).  

demo(T, game(S,F)):- 
	demo(T, \+ final(S)), 
	demo(T, legal(M,S)), 
	demo(T, game(do(M,S),F)).

demo(T, holds(F, T)):-
	demo(T, initially(F)).
demo(T, holds(F, do(M, S))):- 
	demo(T, effect(F, M, S)).
demo(T, holds(F, do(M, S))):- 
	demo(T, holds(F, S)), 
	demo(T, \+ abnormal(F, M, S)).

demo(T, \+ Goal):-
	!,
	\+ demo(T, Goal).
demo(_,Goal):-
	system(Goal),
	!,
	call(Goal).
demo(T, Goal):-
	instance_of(T, Class), % no inheritance currently
	rule(Class, Goal, Body),
	demo_conj(T, Body).




