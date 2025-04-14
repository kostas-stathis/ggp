/* There is no finally/2 defs here, as there is no utility matrix, and so there
are no utilities provided for the final state. Also, there is no control being
introduced, when the initial player loses it. I guess we can live with the first 
error but not with the second, as the game iteration fails. */

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

:- discontiguous initially/2.
:- discontiguous possible/2.
:- discontiguous effect/3.
:- discontiguous abnormal/3.

% Initial state
initial(s0).

% What holds initially
initially(player(p1), s0).
initially(player(p2), s0).
initially(role(p1,x), s0).
initially(role(p2,o), s0).
initially(control(p1), s0).
initially(empty(1,1), s0).
initially(empty(1,2), s0).
initially(empty(1,3), s0).
initially(empty(2,1), s0).
initially(empty(2,2), s0).
initially(empty(2,3), s0).
initially(empty(3,1), s0).
initially(empty(3,2), s0).
initially(empty(3,3), s0).
initially(default_move(_, '1_1'), s0).

% Possible moves - naming convention: 'row_col'
possible(move(P,'1_1'), S):- holds(player(P), S), holds(empty(1,1), S).
possible(move(P,'1_2'), S):- holds(player(P), S), holds(empty(1,2), S).
possible(move(P,'1_3'), S):- holds(player(P), S), holds(empty(1,3), S).
possible(move(P,'2_1'), S):- holds(player(P), S), holds(empty(2,1), S).
possible(move(P,'2_2'), S):- holds(player(P), S), holds(empty(2,2), S).
possible(move(P,'2_3'), S):- holds(player(P), S), holds(empty(2,3), S).
possible(move(P,'3_1'), S):- holds(player(P), S), holds(empty(3,1), S).
possible(move(P,'3_2'), S):- holds(player(P), S), holds(empty(3,2), S).
possible(move(P,'3_3'), S):- holds(player(P), S), holds(empty(3,3), S).

% Effects of moves
effect(mark(X,Y,M), move(P,Pos), S):-
    holds(role(P,M), S),
    position(Pos,X,Y).
effect(control(O), move(P, _), S):-holds(role(P, M), S), holds(role(O,Mo), S), Mo \= M.

% Position conversion
position('1_1',1,1).
position('1_2',1,2).
position('1_3',1,3).
position('2_1',2,1).
position('2_2',2,2).
position('2_3',2,3).
position('3_1',3,1).
position('3_2',3,2).
position('3_3',3,3).

% Abnormal conditions
abnormal(empty(X,Y), move(P,Pos), S):-
    position(Pos,X,Y),
    holds(player(P), S).
abnormal(control(P), move(P,_), S):-
    holds(player(P), S).

% Final state conditions
final(S):- win_state(S).
final(S):- draw_state(S).

% Win conditions for both players
win_state(S):-
    holds(mark(X,1,M), S),
    holds(mark(X,2,M), S),
    holds(mark(X,3,M), S).
win_state(S):-
    holds(mark(1,Y,M), S),
    holds(mark(2,Y,M), S),
    holds(mark(3,Y,M), S).
win_state(S):-
    holds(mark(1,1,M), S),
    holds(mark(2,2,M), S),
    holds(mark(3,3,M), S).
win_state(S):-
    holds(mark(1,3,M), S),
    holds(mark(2,2,M), S),
    holds(mark(3,1,M), S).

% Draw state - all positions filled
draw_state(S):-
    \+ win_state(S),
    \+ (holds(empty(_,_), S)).

% Random strategy
select(P, _O, S, M) :-
    setof(Mi, possible(move(P, Mi), S), Moves),
    random_member(M, Moves).

% Legal moves
legal(move(P, M), S):- 
    possible(move(P, M), S), 
    holds(control(P), S).
