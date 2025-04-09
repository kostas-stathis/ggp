
/*
	Noughts and Crosses XO Game
	author: Kostas Stathis
	created: 02/07/24
	last update: 02/07/24
*/

/* Game independent description */

% All legal evolutions of a game: can be used both as a generator and test.
game(F,F):- final(F).  
game(S,F):- 
	\+ final(S), 
	legal(M,S), %write(M), nl, 
	game(do(M,S),F).

% Situation Calculus - our formulation for games.
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).

% Tic-Tac-Toe domain specific axioms for Prolog using situation calculus

% Initial state
initial(s0).

% Specify what holds initially: all cells on a 3x3 grid are empty and control is given to player X
initially(empty(X, Y), s0) :- between(1, 3, X), between(1, 3, Y).
initially(control('X'), s0).

% Define legal moves
% A legal move consists of marking an unoccupied cell by the player whose turn it is
legal(mark(P, X, Y), S) :- 
    possible(mark(P, X, Y), S),
    holds(control(P), S),
    holds(empty(X, Y), S),
    member(P, ['X', 'O']).

% Define possible moves, which is constrained by the board limits
possible(mark(P, X, Y), S) :- 
    holds(empty(X, Y), S),
    member(P, ['X', 'O']),
    between(1, 3, X), 
    between(1, 3, Y).

% Define the effects of the moves
% If a player P makes a move at position (X, Y), the cell is no longer empty and contains the player's marker
effect(occupied(X, Y, P), mark(P, X, Y), S).
effect(not(empty(X, Y)), mark(_, X, Y), S).

% switching control to the other player
abnormal(control(P), mark(P, _, _), _).
effect(control(Q), mark(P, _, _), _) :- 
    opposite(P, Q),
    member(P, ['X', 'O']),
    member(Q, ['X', 'O']).

% Define opposites to switch turns
opposite('X', 'O').
opposite('O', 'X').

% Win conditions: a player wins if they own any complete row, column, or diagonal
win(P, S) :- row_win(P, S); col_win(P, S); diag_win(P, S).

row_win(P, S) :- holds(occupied(X, 1, P), S), holds(occupied(X, 2, P), S), holds(occupied(X, 3, P), S), between(1, 3, X).
col_win(P, S) :- holds(occupied(1, Y, P), S), holds(occupied(2, Y, P), S), holds(occupied(3, Y, P), S), between(1, 3, Y).
diag_win(P, S) :- (holds(occupied(1, 1, P), S), holds(occupied(2, 2, P), S), holds(occupied(3, 3, P), S));
                  (holds(occupied(1, 3, P), S), holds(occupied(2, 2, P), S), holds(occupied(3, 1, P), S)).

% Define when the board is full: there are no more empty cells remaining
full_board(S) :- \+ holds(empty(_, _), S).

% Final game state: either when a player wins or when the board is completely filled
final(S) :- win('X', S); win('O', S); full_board(S).

% Note: This configuration relies on the domain-independent predicates defined in the general situation calculus framework provided.
