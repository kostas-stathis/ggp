construct_game(pd(Id, P1, P2)):-
    assertz(rule(pd, initially(player(P1), Id),[])),
    assertz(rule(pd, initially(player(P2), Id),[])),
    assertz(rule(pd, initially(control(P1), Id),[])), 
    assertz(rule(pd, initially(control(P2), Id),[])),
    assertz(rule(pd, initially(role(P1,row), Id),[])),
    assertz(rule(pd, initially(role(P2,col), Id),[])),
    assertz(instance_of(Id, pd)).
