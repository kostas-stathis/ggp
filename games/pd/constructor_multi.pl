pd(Id, P1, P2):-
    assert(pd(Id, initially(player(P1))),[]),
    assert(pd(Id, initially(player(P2))),[]),
    assert(pd(Id, initially(control(P1))),[]),
    assert(pd(Id, initially(control(P2))),[]),
    assert(pd(Id, initially(role(P1,row))),[]),
    assert(pd(Id, initially(role(P2,col))),[]).
