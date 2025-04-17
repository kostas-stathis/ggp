pd_call(Id, P1, P2):-
    assertz(pd(initially(player(P1))),[]),
    assertz(pd(initially(player(P2))),[]),
    assertz(pd(initially(control(P1))),[]),
    assertz(pd(initially(control(P2))),[]),
    assertz(pd(initially(role(P1,row))),[]),
    assertz(pd(initially(role(P2,col))),[]).
