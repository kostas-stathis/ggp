:- discontiguous initially/2, effect/3, abnormal/3, legal/2, final/1.

% Domain-independent part of the situation calculus
holds(F, S):- initially(F, S).
holds(F, do(M, S)):- effect(F, M, S).
holds(F, do(A, S)):- holds(F, S), \+ abnormal(F, A, S).

% All legal evolutions of a game: can be used both as a generator and test.
game(F, F):- final(F).  
game(S, F):- 
    \+ final(S), 
    legal(M, S), 
    game(do(M, S), F).

% Domain-specific axioms for the Prisoner's Dilemma
% Define the suspects
suspect(suspect1).
suspect(suspect2).

% Actions legal in a given state
legal(confess(Suspect), _):- suspect(Suspect).
legal(silent(Suspect), _):- suspect(Suspect).

% Initial situation
initially(state(0, 0), s0).

% Define final state
final(state(_, _)).

% Effects of actions
effect(state(Y1, Y2), confess(suspect1), state(NY1, NY2)) :- 
    holds(state(Y1, Y2), s0), 
    NY1 is 0,  % suspect1 goes free
    NY2 is 10. % suspect2 gets 10 years 

effect(state(Y1, Y2), confess(suspect2), state(NY1, NY2)) :- 
    holds(state(Y1, Y2), s0), 
    NY1 is 10, % suspect1 gets 10 years
    NY2 is 0.  % suspect2 goes free

effect(state(Y1, Y2), silent(suspect1), state(NY1, NY2)) :- 
    holds(state(Y1, Y2), s0), 
    ( % If suspect2 also silent
        holds(state(_, _), do(silent(suspect2), s0)) 
    -> NY1 is Y1 + 1, NY2 is Y2 + 1
    ; % If suspect2 confesses
      NY1 is Y1, NY2 is 10
    ).

effect(state(Y1, Y2), silent(suspect2), state(NY1, NY2)) :- 
    holds(state(Y1, Y2), s0), 
    ( % If suspect1 also silent
        holds(state(_, _), do(silent(suspect1), s0)) 
    -> NY1 is Y1 + 1, NY2 is Y2 + 1
    ; % If suspect1 confesses
      NY1 is 10, NY2 is Y2
    ).

effect(state(Y1, Y2), confess(suspect1), state(NY1, NY2)):-
    holds(state(Y1, Y2), s0),
    ( % If suspect2 also confesses
        holds(state(_, _), do(confess(suspect2), s0))
    -> NY1 is 5, NY2 is 5
    ; NY1 is 0, NY2 is 10 %Otherwise suspect2 gets 10
    ).

effect(state(Y1, Y2), confess(suspect2), state(NY1, NY2)):-
    holds(state(Y1, Y2), s0),
    ( % If suspect1 also confesses
        holds(state(_, _), do(confess(suspect1), s0))
    -> NY1 is 5, NY2 is 5
    ; NY1 is 10, NY2 is 0 %Otherwise suspect1 gets 10
    ).

% No abnormal situations in this simple implementation
abnormal(_, _, _) :- false.
