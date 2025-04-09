/*
	Game description framework 
	author: Kostas Stathis
	SWI-Prolog version
	Last update: 12/05/2024
*/

/* Game independent description */

% All legal evolutions of a game: can be used both as a generator and test.
game(F,F):- final(F).  
game(S,F):- \+ final(S), legal(M,S), game(do(M,S),F).

% Situation Calculus - our formulation for games.
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).


/* Game specific predicates for Prisoner's Dilemma (PD) */

%Action names for defect and cooperate.
defect('D').
coop('C').

% Payoff matrix for PD
payoffPD(D, D, 2, 2):-defect(D).
payoffPD(C, D, 1, 4):-coop(C),defect(D).
payoffPD(D, C, 4, 1):-defect(D),coop(C).
payoffPD(C, C, 3, 3):-coop(C).

% Initial state
initial(s0).

% What holds initially: who is a player, their role, and whether they can play.	
initially(player(p1), s0).
initially(player(p2), s0).
initially(role(p1,row), s0).
initially(role(p2,col), s0).
initially(control(p1), s0).
initially(control(p2), s0).

% When a state is final: no need to check content as it generated by game/2,
% implying it is a legal state.
final(do(choice(_P2, _M2), do(choice(_P1, _M1), S))):-initial(S).

% A legal move is a possible move where the player has control (is its turn).
legal(choice(P, M), S):- possible(choice(P, M), S), holds(control(P), S).

% What is possible for a player to choose
possible(choice(P,D), S):- defect(D), holds(player(P), S).
possible(choice(P,C), S):- coop(C), holds(player(P), S).

% The effects of a move: if P has chosen M, then in the next state this
% is what they did.
effect(did(P, M), choice(P, M), _S).

% The effects of a move: once a choice is made, the player looses control, i.e.
% cannot move anymore.
abnormal(control(P), choice(P, _M), _S).

% What holds finally: the outcome with players, Moves, and Utilities.
finally(outcome(P1,M1,U1,P2,M2,U2), S):-
	final(S),
	holds(role(P1, row), S),	
	holds(did(P1, M1), S),	
	holds(role(P2, col), S),	
	holds(did(P2, M2), S),
	payoffPD(M1, M2, U1, U2).

% Goals achieved by the players.
finally(goal(P1, U1), S):-
	finally(outcome(P1,_,U1,_,_,_), S).
finally(goal(P2, U2), S):-
	finally(outcome(_,_,_,P2,_,U2), S).