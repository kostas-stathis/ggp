compose(Name):-
	atomic_list_concat(['./',comps,'/', Name,'_comp'], Composition), 
	consult(Composition),

	solver_name(S),
	atomic_list_concat(['./',solver,'/', S], SolverDesc), 
	consult(SolverDesc).

	game_name(G),
	atomic_list_concat(['./',games,'/', G], GameDesc), 
	consult(GameDesc).
