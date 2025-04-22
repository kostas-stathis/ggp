construct_game(pd(Id, P1, P2)):-
    assertz(rule(pd, initially(player(P1)),[])),
    assertz(rule(pd, initially(player(P2)),[])),
    assertz(rule(pd, initially(control(P1)),[])), 
    assertz(rule(pd, initially(control(P2)),[])),
    assertz(rule(pd, initially(role(P1,row)),[])),
    assertz(rule(pd, initially(role(P2,col)),[])),
    assertz(instance_of(Id, pd)).
