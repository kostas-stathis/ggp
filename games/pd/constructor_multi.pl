pd_call(Id, P1, P2):-
    assertz(demo(Id, pd, initially(player(P1)))),
    assertz(demo(Id, pd, initially(player(P2)))),
    assertz(demo(Id, pd, initially(control(P1)))),
    assertz(demo(Id, pd, initially(control(P2)))),
    assertz(demo(Id, pd, initially(role(P1,row)))),
    assertz(demo(Id, pd, initially(role(P2,col)))).
