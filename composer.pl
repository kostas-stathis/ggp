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

consult_solver(file=Solver, mode=M):-
	load_comp(type=generic, dir=solvers, mode=M, file=Solver).

consult_lib(file=L, mode=M):-
	load_comp(type=generic, dir=lib, mode=M, file=L).

consult_rules(game=Name, mode=M, file=Rules):-
	load_comp(game=Name, dir=games, mode=M, file=Rules).

consult_utility(game=Name, mode=M, file=Utility):-
	load_comp(game=Name, dir=games, mode=M, file=Utility).

consult_initial(game=Name, id=Id, player=P1, opponent=P2, file=Constructor, mode=M):-
	load_comp(game=Name, dir=games, mode=M, file=Constructor),
,
	GameInstance =.. [Name, Id, P1, P2],
	construct_game(GameInstance).

	
consult_config(C):-
	configuration(C, Config),
	% consult game framework
	member(solver=S, Config),
	member(lib=L, Config),
	member(mode=M, Config),
	consult_framework(solver=S, lib=L, mode=M),
	% consult game
	member(game=Name, Config),
	member(player=P1, Config),
	member(opponent=P2, Config),
	member(id=Id, Config),
	member(rules=R, Config),
	member(utility=U, Config),
	member(constructor=Ci, Config),
	consult_game(game=Name, id=Id, player=P1, opponent=P2, rules=R, utility=U, constructor=Ci, mode=M).

consult_framework(solver=S, lib=L, mode=M):-
	consult_solver(file=S, mode=M),
	consult_lib(file=L,	mode=M).

consult_game(game=Name, id=Id, player=P1, opponent=P2, rules=R, utility=U, constructor=C, mode=M):-
	consult_rules(game=Name, mode=M, file=R),
	consult_utility(game=Name, mode=M, file=U),
	consult_initial(game=Name, id=Id, player=P1, opponent=P2, file=C, mode=M).

% Here are the configurations we need to test
configuration(1,[
				game = pd, 
				id = s0, 
				player = p1, 
				opponent = p2,
				solver = single_game_sc,
				lib=printing,
				rules= rules_single,
				utility=utility_single,
				constructor=constructor_single,
				mode=c	
				]).

configuration(2,[
				game = pd, 
				id = s0, 
				player = p1, 
				opponent = p2,
				solver = multi_game_sc,
				lib=printing,
				rules= rules_multi,
				utility=utility_multi,
				constructor=constructor_multi,
				mode=c		
				]).

?-consult_config(2).