

rule(pd, final(do(move(_P2, _M2), do(move(_P1, _M1), S))), 
	[
		atomic(S)
	]).

rule(pd, legal(move(P, M), S), 
	[
		possible(move(P, M), S), 
		holds(control(P), S)
	]).

rule(pd, possible(move(P,M), S), 
	[
		available(move(P, M), S),
		holds(player(P), S)
	]).

rule(pd, available(move(_, 'D'),_), []).
rule(pd, available(move(_, 'C'),_), []).

rule(pd, effect(did(P, M), move(P, M), _S),[]).

rule(pd, abnormal(control(P), move(P, _M), _S),[]).

rule(pd, finally(outcome(P1,M1,U1,P2,M2,U2), S),
	[
		final(S),
		holds(role(P1, row), S),	
		holds(did(P1, M1), S),
		holds(role(P2, col), S),	
		holds(did(P2, M2), S),
		payoff(M1, M2, U1, U2)
	]).

rule(pd, finally(goal(P1, U1), S),
	[
		finally(outcome(P1,_,U1,_,_,_), S)
	]).
rule(pd, finally(goal(P2, U2), S),
	[
		finally(outcome(_,_,_,P2,_,U2), S)
	]).