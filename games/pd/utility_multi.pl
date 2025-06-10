% Payoff matrix for PD
rule(pd, payoff('D', 'D', 2, 2),[]).
rule(pd, payoff('C', 'D', 1, 4),[]).
rule(pd, payoff('D', 'C', 4, 1),[]).
rule(pd, payoff('C', 'C', 3, 3),[]).