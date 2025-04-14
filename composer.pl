% --- Directory structure
solvers_dir('./solvers').
games_dir('./games/').
strategies_dir('./strategies').
players_dir('./players/').
lib_dir('./lib').



%% load_solver(+Mode, +FileName)
%  Loads a solver component from the solvers directory using Mode (e.g., c for consult, rc for reconsult).
load_solver_component(Mode, FileName) :-
	solvers_dir(Dir),
	load_component(Dir, FileName, Mode).

%% load_lib_component(+Mode, +FileName)
%  Loads a library component from the lib directory using Mode (e.g., c for consult, rc for reconsult).
load_lib_component(Mode, FileName) :-
	lib_dir(Dir),
	load_component(Dir, FileName, Mode).

%% load_component(+Dir, +FileName, +Mode)
%  Constructs full path and consults or reconsults the file based on the given Mode.
load_component(Dir, FileName, c) :-
    atomic_list_concat([Dir, FileName, '.pl'], Path),
    consult(Path).

load_component(Dir, FileName, rc) :-
    atomic_list_concat([Dir, FileName, '.pl'], Path),
    reconsult(Path).

?- load_lib_component(c, 'printing').
?- load_solver_component(c, 'sc_solver').

/*

consult_components([]).
consult_components([H|T]):-
	consult_component(H),!,
	consult_components(T).

consult_component(game=Game):-	
		consult_game(Game).

consult_component(rules=Rules):-
		consult_rules(Rules).

consult_game(Game):-
	atomic_list_concat(['./',games,'/', Solver], SolverDesc),
		consult(SolverDesc),
		atomic_list_concat(['./',games,'/', Game], GameDesc),
		consult(GameDesc),
		atomic_list_concat(['./',games,'/', Rules], RulesDesc),
		consult(RulesDesc)
	atomic_list_concat(['./',games,'/', Game], GameDesc),
	consult(GameDesc),
*/

/* Composition contains the 
- domain independent solver and library, 
- the game dependent rules, 
- and the initial instance of the game

composition(pd:c1, [rules=spec1]).
composition(pd:c2, [game=pd, rules=spec2, lib=games_lib]).
*/