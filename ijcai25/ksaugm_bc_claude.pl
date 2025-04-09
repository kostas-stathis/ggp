/* This is pretty good. It is almost correct. The only thing is that it assumes that possible moves only test for a move that it is not a number, but it comes from an interface, as an atom. Really interesting. Works perfectly otherwise.*/

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


/* Game specific predicates for 70% of Average Game */

:- discontiguous effect/3.
:- discontiguous initially/2.
:- discontiguous finally/2.

% Initial state
initial(s0).

% What holds initially
initially(player(p1), s0).
initially(player(p2), s0).
initially(role(p1, first), s0).
initially(role(p2, second), s0).
initially(control(p1), s0).
initially(control(p2), s0).
initially(default_move(_, '50'), s0).  % middle value as default

% Final state definition
final(do(move(_P2, _M2), do(move(_P1, _M1), S))):-initial(S).

% Legal moves
legal(move(P, M), S):- possible(move(P, M), S), holds(control(P), S).

% Possible moves (numbers from 0 to 100)
possible(move(P,M), S):- 
    holds(player(P), S),
    ks_in_range(0, 101, M, N), % added this, assumes possible tests only, i.e. it does not generate.
    N >= 0,
    N =< 100.

ks_in_range(N1, N2, M, N):- random(N1, N2, N), atom_number(M,N).

% Effect of moves
effect(did(P, M), move(P, M), _S).

% Control changes after a move
abnormal(control(P), move(P, _M), _S).

% Calculate target value (70% of average)
calculate_target(M1, M2, Target) :-
    atom_number(M1, N1),
    atom_number(M2, N2),
    Avg is (N1 + N2) / 2,
    Target is Avg * 0.7.

% Calculate distance from target
distance(Num, Target, Dist) :-
    atom_number(Num, N),
    Dist is abs(N - Target).

% Determine utilities based on distances from target
determine_utilities(D1, D2, U1, U2) :-
    (D1 < D2 -> (U1 = 1, U2 = 0);
     D2 < D1 -> (U1 = 0, U2 = 1);
     D1 = D2 -> (U1 = 0.5, U2 = 0.5)).

% Final outcome
finally(outcome(P1,M1,U1,P2,M2,U2), S):-
    final(S),
    holds(role(P1, first), S),
    holds(did(P1, M1), S),
    holds(role(P2, second), S),
    holds(did(P2, M2), S),
    calculate_target(M1, M2, Target),
    distance(M1, Target, D1),
    distance(M2, Target, D2),
    determine_utilities(D1, D2, U1, U2).

% Goals achieved by players
finally(goal(P1, U1), S):-
    finally(outcome(P1,_,U1,_,_,_), S).
finally(goal(P2, U2), S):-
    finally(outcome(_,_,_,P2,_,U2), S).

% Random strategy
select(P, _O, S, M) :-
    random(0, 101, N),  % generates random number 0-100
    atom_number(N, M),
    possible(move(P, M), S).
