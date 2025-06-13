/* 

Author: Kostas Stathis
Date: 2023-10-01
This file contains the implementation of the find_failure/2 predicate.
This predicate is used to find failures in a Prolog goal.
Usage:
?- find_failure(p(X), Failure).
*/

% This file is part of the Failure Finder library.
explain_failure(Goal, uknown(Goal)):-
    \+ defined(Goal), !.
explain_failure(Goal, Goal:([], [false], [])) :-
    defined(Goal),
    \+ clause(Goal, _), !.
explain_failure(Goal, Goal:([], [], [])) :-
    defined(Goal), !,
    clause(Goal, Body),
    explain_failures(Body, ExplanationFailure).

explain_failures(true, []).

/* I think it is important to have the following

Head: Succeeded, Failure, Untried.

For Failure we need the deep failure.

*/

defined(Goal) :-
    functor(Goal, Functor, Arity),
    current_predicate(Functor/Arity).


p(X):- q(X), r(X).

q(a).
q(b).