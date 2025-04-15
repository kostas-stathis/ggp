% --- Directory structure
dir(solvers, './solvers/').
dir(games, './games/').
dir(strategies, './strategies/').
dir(players, './players/').
dir(lib, './lib/').

%%load_component(mode=M, type=T))
load_comp(type=generic, dir=D, mode=M, file=File):-
	dir(D, Dir),
	atomic_list_concat([Dir, File], Path),
	load_file(Path, M).
load_comp(game=Name, dir=D, mode=M, file=File):-
	dir(D, Dir),
	atomic_list_concat([Dir, Name,'/', File], Path),
	load_file(Path, M).

load_file(Path, c) :- !, consult(Path).
load_file(Path, rc) :- reconsult(Path).

consult_solver(file=Solver):-
	load_comp(type=generic, dir=solvers, mode=c, file=Solver).

consult_lib(file=L):-
	load_comp(type=generic, dir=lib, mode=c, file=L).

consult_rules(game=Name):-
	load_comp(game=Name, dir=games, mode=c, file=rules),
	load_comp(game=Name, dir=games, mode=c, file=utility).

consult_initial(Name, Id, P1, P2):-
	load_comp(game=Name, dir=games, mode=c, file=constructor),
	Constructor =.. [Name, Id, P1, P2],
	call(Constructor).

consult_game(Name, Id, P1, P2):-
	consult_lib(file=printing),
	consult_solver(file=sc_solver),
	consult_rules(game=Name),
	consult_initial(Name, Id, P1, P2).

?- consult_game(pd, s0, p1, p2).