/* 

Author: Kostas Stathis
Date: 2023-10-01
This file contains the implementation of the find_failure/2 predicate.
This predicate is used to find failures in a Prolog goal.
Usage:
?- find_failure(p(X), Failure).
*/

% This file is part of the Failure Finder library.
goal_failure(Goal, ):-
    \+ defined(Goal), !.

find_failure(Goal, Goal:(Succeeded, Failed, Untried)) :-
    defined(Goal), !,
    clause(Goal, Body),
    find_failures(Body, Succeeded, Failed, Untried).

/* I think it is important to have the following

Head: Succeeded, Failure, Untried.

For Failure we need the deep failure.

*/

find_failures(true, [true], [], []) :- !.

find_failures((Head,Tail), [Head|Rest], F, Untried) :-
    defined(Head),
    find_failure(Head, HeadFailure),
    find_failures(Tail, TailFailures).

defined(Goal) :-
    functor(Goal, Functor, Arity),
    current_predicate(Functor/Arity).


p(X):- q(X), r(X).

q(a).
q(b).