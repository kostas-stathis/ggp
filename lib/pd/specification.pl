/*
	Prisoners dilemma specification
	author: Kostas Stathis
	created: 7/12/23
	last update: 30/03/24
*/

/* Game dependent predicates                 
 -----------------------------------------------
 Representation assumes SC like assertions as logical
 terms. 

 No need to introduce initial states, as we assume that 
 we do not have to play multiple games at once. 

 Effects and results are achieved via unification.
 ------------------------------------------------- 
*/

% ~20 lines of Prolog

% Specific to pd
isa(pd, s0).
/*
template(['We have player ', P1, ' as the first player and player ', P2, ' as the second player. Give me some explanation to how you arrived to this point.']):-
	initially(player(P1), s0),
	initially(role(P1,row), s0),
	initially(player(P2), s0),
	initially(role(P2,col), s0).
*/
	
initially(player(p1), s0).
initially(player(p2), s0).
initially(role(p1,row), s0).
initially(role(p2,col), s0).

defect('D').
coop('C').

payoff(D, D, 2, 2):-defect(D).
payoff(C, D, 1, 4):-coop(C),defect(D).
payoff(D, C, 4, 1):-defect(D),coop(C).
payoff(C, C, 3, 3):-coop(C).

% The following predicates hold for any symmetric game *****

% When a state is final
final(S):-
	holds(played(P1), S),
	holds(role(P1, row), S),
	holds(played(P2), S),
	holds(role(P2, col), S),
	P1 \= P2.

% When a move is legal
legal(select(P, M), S):- available(select(P, M), S), \+ holds(played(P), S).

available(select(P,D), S):- isa(pd, S), defect(D), holds(player(P), S).
available(select(P,C), S):- isa(pd, S), coop(C), holds(player(P), S).

% The effects of a move
effect(played(P), select(P, _M), _S).
effect(move(P, M), select(P, M), _S).

% What holds finally
finally(encounter(P1,M1,U1, P2,M2,U2), S):-
	holds(move(P1, M1), S),	
	holds(role(P1, row), S),
	holds(move(P2, M2), S),
	holds(role(P2, col), S),
	P1 \= P2,
	holds(payoff(M1, M2, U1, U2), S).

finally(util(P1, U1), S):-
	finally(encounter(P1, _, U1, _, _, _), S).
finally(util(P2, U2), S):-
	finally(encounter(_, _, _, P2, _, U2), S).



/*
do(A,S)

s0, do(sel(p1,d), s0), do(sel(p2,c), do(sel(p1,d), s0))

do(sel(p2,c), do(sel(p1,d), s0))

strong_option(S):- S=do(sel(p2,c), do(sel(p1,c), s0));S=do(sel(p1,c), do(sel(p2,c), s0)).
significant_payoff(S,P):-holds(player(P), S), finally(util(P,U), S), U > 50.

temptation(S):- S=do(sel(p2,X), do(sel(p1,b), s0)),

fin

*/
