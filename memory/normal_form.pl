/*  
	Payoff matrix/normal form representation 
	No rules of the game are provided, only 
        the payoff matrix description.
*/

% Outcome representation - requires either a game maneger
% to give the moves, or an abductive procedure to hypothesize
% what the players will in different scenarios

outcome(R1, A1, R2, A2):- does(R1,A1), does(R2,A2).

% payoffs of individual players, deduced from concrete outcomes
% (whether obtained from the actual moves of the players, or from
% the hypotheses of the abductive procedure).

payoff(R1, defect, 5):- opponent_of(R1,R2), outcome(R1, defect, R2, coop).
payoff(R1, coop, 0):- opponent_of(R1,R2), outcome(R1, coop, R2, defect).
payoff(R1, defect, 1):- opponent_of(R1,R2), outcome(R1, defect, R2, defect).
payoff(R1, coop, 3):- opponent_of(R1,R2), outcome(R1, coop, R2, coop).

% Roles of the players
role(row).
role(col).

% What is an opponent
opponent_of(R1, R2):- role(R1), role(R2), R1 \= R2.

% ----------------------------------------------------------------
% Player usage knowing their role.


?- goal(row, X).
