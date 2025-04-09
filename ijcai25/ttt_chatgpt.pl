/* Generate and test problem. Requires more substantial debufgging. */


:- dynamic initially/2.

% All legal evolutions of a game: can be used both as a generator and test.
game(F,F):- final(F).
game(S,F):- \+ final(S), legal(M,S), game(do(M,S),F).


% The domain independent version of the situation calculus is as follows:

% Situation Calculus - our formulation for games.
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).

% (Re)initialise the state
initialise(Target, State):-
    Target =.. [Pred, Id, _],
    Current =.. [Pred, Id, _],
    (initially(Current, State) -> retract(initially(Current, State)); true),
    assert(initially(Target, State)).
 
:- discontiguous initial/1.
:- discontiguous initially/2.
:- discontiguous final/1.
:- discontiguous legal/2.
:- discontiguous possible/2.
:- discontiguous effect/3.
:- discontiguous abnormal/3.
:- discontiguous finally/2.

/* Game specific predicates for Tic-Tac-Toe (TTT) */

% Initial state
initial(s0).

% What holds initially: who is a player, their role, and whether they can play.
initially(player(p1), s0).
initially(player(p2), s0).
initially(role(p1, x), s0).
initially(role(p2, o), s0).
initially(control(p1), s0).
initially(default_move(empty, 'x'), s0).

% A legal move is a possible move where the player has control (is its turn).
legal(move(P, Position), S):- possible(move(P, Position), S), holds(control(P), S).

% What is possible for a player to choose
possible(move(P, Position), S):- holds(player(P), S), holds(empty(Position), S).

% Moves that are opposite to each other
opposite_move('x', 'o').
opposite_move('o', 'x').

% The effects of a move: if P places a mark on a position, the position is no longer empty.
effect(occupied(Position, Mark), move(P, Position), S):- holds(role(P, Mark), S).

% The effects of a move: once a move is made, change control to the other player.
effect(control(NextPlayer), move(_, _), S):- opposite_move(NextPlayer, _), holds(player(NextPlayer), S).

% A state is final if a player has won or the board is full without a winner.
final(S):- win(_, S).
final(S):- full_board(S), \+ win(_, S).

% Define win condition
win(P, S):- holds(role(P, Mark), S), line_completed(Mark, S).

% Helper predicates to check if a line is completed
line_completed(Mark, S):- row_completed(Mark, S).
line_completed(Mark, S):- column_completed(Mark, S).
line_completed(Mark, S):- diagonal_completed(Mark, S).

% Check if a row is completed
row_completed(Mark, S):- holds(occupied(pos(1, Y), Mark), S), holds(occupied(pos(2, Y), Mark), S), holds(occupied(pos(3, Y), Mark), S).

% Check if a column is completed
column_completed(Mark, S):- holds(occupied(pos(X, 1), Mark), S), holds(occupied(pos(X, 2), Mark), S), holds(occupied(pos(X, 3), Mark), S).

% Check if a diagonal is completed
diagonal_completed(Mark, S):- holds(occupied(pos(1, 1), Mark), S), holds(occupied(pos(2, 2), Mark), S), holds(occupied(pos(3, 3), Mark), S).
diagonal_completed(Mark, S):- holds(occupied(pos(1, 3), Mark), S), holds(occupied(pos(2, 2), Mark), S), holds(occupied(pos(3, 1), Mark), S).

% Check if the board is full (i.e., no empty positions remain)
full_board(S):- forall(pos(X,Y), \+ holds(empty(pos(X, Y)), S)).

% Define helper predicate to iterate over board positions
pos(1, 1). pos(1, 2). pos(1, 3).
pos(2, 1). pos(2, 2). pos(2, 3).
pos(3, 1). pos(3, 2). pos(3, 3).
