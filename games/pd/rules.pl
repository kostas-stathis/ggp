
final(do(move(_P2, _M2), do(move(_P1, _M1), pd(S)):-initial(S).

% A legal move is a possible move where the player has control (is its turn).
legal(move(P, M), S):- possible(move(P, M), S), holds(control(P), S).

% What is possible for a player to choose
possible(move(P,'D'), S):- holds(player(P), S).
possible(move(P,'C'), S):- holds(player(P), S).

% The effects of a move: if P has chosen M, then in the next state this
% is what they did.
effect(did(P, M), move(P, M), _S).

% The effects of a move: once a choice is made, the player looses control, i.e.
% cannot move anymore.
abnormal(control(P), move(P, _M), _S).

% What holds finally: the outcome with players, Moves, and Utilities.
finally(outcome(P1,M1,U1,P2,M2,U2), S):-
	holds(role(P1, row), S),	
	holds(did(P1, M1), S),	
	holds(did(P2, M2), S),
	payoffPD(M1, M2, U1, U2).

% Goals achieved by the players.
finally(goal(P1, U1), S):-
	finally(outcome(P1,_,U1,_,_,_), S).
finally(goal(P2, U2), S):-
	finally(outcome(_,_,_,P2,_,U2), S).