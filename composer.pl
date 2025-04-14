% Generic framework with library loaded
:- solver(consult).
:- lib(consult).


% Directory structure
solver_dir('./solvers/').
games_dir('./games/').
strategies_dir('./strategies/').
players_dir('./players/').
lib_dir('./lib/').


solver(Mode, FileName):-
		solver_dir(Dir),
		load_component(Dir, FileName, Mode).

lib(Mode, FileName):-
		lib_dir(Dir),
		load_component(Dir, FileName, Mode).

load_component(Dir, FileName, Mode):-	
		(
			exists_directory(Dir) 
			-> 
			atomic_list_concat([Dir, FileName], SolverDesc),
			(
				exists_file(SolverDesc)
				-> 
				(
					Mode = consult
					-> 
						consult(SolverDesc)
					; 
						reconsult(SolverDesc)
				)
				; 
				writelistnl([SolverDesc,' does not exist!')
			)
			;
			writelistnl([Dir,' does not exist!'])
		).



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
*/
composition(pd:c1, [rules=spec1]).
composition(pd:c2, [game=pd, rules=spec2, lib=games_lib]).
