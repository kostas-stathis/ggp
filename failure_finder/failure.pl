find_failure(Goal, undefined(Goal)):-
    undefined(Goal), !.

find_failure(Goal, failed(Goal)) :-
     defined(Goal), !,
    \+ call(Goal).

find_failure(Goal, Failure) :-
    clause(Goal, Body),
    find_failures(Body, BodyFailures).
