
/* Reasoning predicates
 --------------------------------------------
*/

% Find the minimum utility when playing a strategy
minU(M, U):-
	findall(X, payoff(M, _, X, _), Xs),
	sort(Xs, [U|_]).
	
% A strategy is safer from another if its min utility 
% is greater that the min utility of the other
safer(M1, M2):-
	minU(M1, U1),
	minU(M2, U2),
	U1 > U2.


/* ==== To check/expand --> at the moment what follows is close to junk
nash(S, P1, M1, P2, M2):-
	best_response_R(S, P1, M1),
	best_response_C(S, P2, M2).

choices(S, Choices):-
	findall(Up-Mp, game(pd(P, O, Mp, Mo, Up, Uo)), L),
	keysort(L, NL),
	reverse(NL, Choices).

% Assuming opponent plays Mo, which move Mp should I play with 
% at least as good utility as his?

% Given a move configuration from the opponent(s), what is the
% move that will give you the best response?
%
% Generate a response.
best_response_to(S, M2, M1):-
	holds(player(P1), S),
	holds(role(P1, row), S),
	holds(player(P2), S),
	holds(role(P2, col), S),
	findall(U1-M1,
		(game(S, F), 
		 holds(move(P2, M2),F), 
		 holds(encounter(P1, M1, U1, P2, M2, U2), F)
		),
	L),
	keysort(L, NL),
	reverse(NL, Choices),
	best_choice(Choices, M1).


best_choice([_-M|_], M).


test0(S, L):- 
	holds(role(P1, row), S),
	holds(role(P2, col), S),
	setof([Up-Mp]:M2, (game(S, F), holds(move(P2, M), F), holds(encounter(P1, Mp, Up, P2, M2, U2), F)), L).

test(S, L):- findall(Up-Mp, (game(S, F), holds(role(P, col), F), holds(move(P, M),F), holds(encounter(P1, Mp, Up, P, M, U), F)), L).

======== */
