/*
	Game of chicken specification
	author: Kostas Stathis
	created: 7/12/23
	last update: 9/12/23
*/

/* Game dependent predicates                 
 -----------------------------------------------
 Representation assumes single game states 
 as Prolog terms. The term

	gc(P,O,Mp,Mo,Up,Uo)

 contains the player P, opponent O, move of P
 is Mp, move of opponent O is Mo, and Up, Uo
 are the player's amd the opponents utilities.

 No need to introduce initial states, or ids,
 as we assume that we do not have to play multiple
 games at once. 

 Effects and results are achieved via unification.
 ------------------------------------------------- 
*/

% ~20 lines of Prolog

terminating(gc(P,O,Mp,Mo,Up,Uo)):-
	ground(gc(P,O,Mp,Mo,Up,Uo)).

payoff('D', 'D', 1, 1).
payoff('C', 'D', 2, 4).
payoff('D', 'C', 4, 2).
payoff('C', 'C', 3, 3).

legal(S, enc(P:Mp,O:Mo)):-
	legal(S, select(P, Mp)),
	legal(S, select(O, Mo)),
	P \= O.

legal(gc(P,_,Mp,_,_,_), select(P, Mp)):-
	available(gc(P,_,Mp,_,_,_), Mp).
legal(gc(_,O,_,Mo,_,_), select(O, Mo)):-
	available(gc(_,O,_,Mo,_,_), Mo).
	
available(gc(_,_,_,_,_,_), 'D').
available(gc(_,_,_,_,_,_), 'C').

effects(gc(P,O,Mp,Mo,Up,Uo),  
        enc(P:Mp, O:Mo),
        gc(P,O,Mp,Mo,Up,Uo)
):-
	payoff(Mp, Mo, Up, Uo).
