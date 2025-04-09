/*
	Noughts and Crosses XO Game
	author: Kostas Stathis
	created: 02/07/24
	last update: 09/07/24
*/

/* Game independent description */

% All legal evolutions of a game: can be used both as a generator and test.
game(F,F):- final(F).  
game(S,F):- 
	\+ final(S), 
	legal(M,S), once((count(S,C),write(C:M), nl)), 
	game(do(M,S),F).

% Situation Calculus - our formulation for games.
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).


% Initial state of a new game
initially(player(p1), s0).
initially(player(p2), s0).
initially(role(p1,o), s0).
initially(role(p2,x), s0).
initially(control(p1), s0).

% Initialisation of the board
initially(pos((1,1),nil), s0).
initially(pos((1,2),nil), s0).
initially(pos((1,3),nil), s0).
initially(pos((2,1),nil), s0).
initially(pos((2,2),nil), s0).
initially(pos((2,3),nil), s0).
initially(pos((3,1),nil), s0).
initially(pos((3,2),nil), s0).
initially(pos((3,3),nil), s0).

% When a state is final
final(S):-
	winning(S),!.
final(S):-
	draw(S).

% Winning state
winning(S):- 
	win_right_diagonal(S),!.
winning(S):-           
	win_left_diagonal(S),!.
winning(S):-
	win_row(S),!.
winning(S):-
	win_column(S),!. 

% Draw is used only if winning has failed.
draw(S):-
	\+ holds(pos(_X, nil), S).

% When a move is legal
legal(M, S):- available(M, S), possible(M, S).


possible(select(P,mark(Cs, _X)), S):- holds(control(P), S), holds(pos(Cs, nil), S). %,\+ in(select(P,mark(Cs, X)),S).

available(select(P,mark((X,Y), M)), S):- holds(player(P), S), holds(role(P, M), S),in_range(X),in_range(Y).

in_range(1).
in_range(2).
in_range(3).


in(Mi, do(Mi,_S)):-!.
in(Mi, do(_Mj,S)):- in(Mi, S).
	
	
% when a player makes a move _M, then they lose control
abnormal(control(P), select(P, _M), _S).
abnormal(pos(X, nil), select(_P, mark(X,_M)), _S).

% The effects of a move
effect(pos((X,Y), M), select(_P, mark((X,Y),M)), _S).
effect(control(O), select(_P, mark((_X,_Y),M)), S):-holds(role(O,Mo), S), Mo \= M.

% What holds finally
finally(util(P1, M1, P2, M2, U1, U2), S):-
	mark_of(S, M1),
	holds(role(P1, M1), S),
	holds(role(P2, M2), S),
	P1 \== P2,
	(winning(S) -> (U1 = 1, U2 = -1) ; (U1 = 0, U2 = 0)).

mark_of(do(select(_,mark(_,M)),_), M).

win_right_diagonal(S):-
	mark_of(S, M),
	forall(in_range(N), holds(pos((N,N), M), S)).
win_left_diagonal(S):-
	mark_of(S, M),
	forall((in_range(X),Y is 4 - X), holds(pos((X,Y),M), S)).

win_row(S):-
	mark_of(S, M),
	once( 
		(in_range(N),
		 holds(pos((N,1),M), S),
		 holds(pos((N,2),M), S),
		 holds(pos((N,3),M), S)
		)
	).

win_column(S):-
	mark_of(S, M),
	once( 
		(in_range(N), 
		 holds(pos((1,N),M),S),
		 holds(pos((2,N),M),S),
		 holds(pos((3,N),M),S)
		)
	).



% Counting

count(do(_, S), N):-
	count(S, NI),
	N is NI + 1.
count(_,0).
 
% Printing
pp(do(M,S)):-
	!,
	pp(S),
	write(M),nl.
pp(_).

update(S, F):-
	%pp(S),
	listinitial(L),
	update_list(S, L, F).
	
update_list(do(M, S),L,FL):-
	update_one(M, L, IL),
	!,
        update_list(S, IL, FL).
update_list(_,L,L).

listinitial([['_','_','_'],['_','_','_'],['_','_','_']]).

update_one(select(_P, mark((X,Y),M)),S,NewS):-
	append(X1,[Y1|Z1], S),
	length([Y1|X1], X),
	append(X2, [Y2|Z2], Y1),
	length([Y2|X2], Y),
	append(X2, [M|Z2], UY1),
	append(X1, [UY1|Z1], NewS),!.

% Queries
query(S):-
	game(S, F),
	finally(R, F),
	update(F, LF),
	forall(member(X,LF),(write(X),nl)),
	write(R),nl,nl.
