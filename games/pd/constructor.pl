pd(Id, P1, P2):-
    assert(initial(Id)),
    assert(initially(player(P1), Id)),
    assert(initially(player(P2), Id)),
    assert(initially(control(P1), Id)),
    assert(initially(control(P2), Id)),
    assert(initially(role(P1,row), Id)),
    assert(initially(role(P2,col), Id)).
