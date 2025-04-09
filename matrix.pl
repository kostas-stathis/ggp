
/*
	Game description framework 
	author: Kostas Stathis
	SWI-Prolog version
	Last update: 12/05/2024
*/

/* Game independent description */

% Payoff matrix for PD
payoffPD(D, D, 2, 2):-defect(D).
payoffPD(C, D, 1, 4):-coop(C),defect(D).
payoffPD(D, C, 4, 1):-defect(D),coop(C).
payoffPD(C, C, 3, 3):-coop(C).

payoff(row('D',2), column('D',2)).
payoff(row('C',1), column('D',2)).
