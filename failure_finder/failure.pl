find_failure(Goal, undefined(Goal)):-
    undefined(Goal), !.

find_failure(Goal, failed(Goal)) :-
     defined(Goal), !,
    \+ call(Goal).

find_failure(Goal, Failure) :-
    clause(Goal, Body),
    find_failures(Body, BodyFailures).

find_failures([], BodyFailures) :-!.
find_failures([Head|Tail], [HeadFailure|TailFailures]) :-
    find_failure(Head, HeadFailure),
    find_failures(Tail, TailFailures).
