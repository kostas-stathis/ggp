% Define the game matrix here. Each cell is of the form cell(Row, Column, PayoffPlayer1, PayoffPlayer2).
% Example 2x2 game matrix:
cell(1, 1, 2, 2).
cell(1, 2, 0, 3).
cell(2, 1, 3, 0).
cell(2, 2, 1, 1).

% Determines the best response for Player 1 given Player 2's strategy (Column)
best_response_p1(Column, BestRows) :-
    findall(Payoff-Row, cell(Row, Column, Payoff, _), Payoffs),
    max_member(MaxPayoff-_, Payoffs),
    findall(R, member(MaxPayoff-R, Payoffs), BestRows).

% Determines the best response for Player 2 given Player 1's strategy (Row)
best_response_p2(Row, BestColumns) :-
    findall(Payoff-Column, cell(Row, Column, _, Payoff), Payoffs),
    max_member(MaxPayoff-_, Payoffs),
    findall(C, member(MaxPayoff-C, Payoffs), BestColumns).

% Example queries:
% To find the best response for Player 1 given Player 2 chooses strategy 1 (Column 1):
% best_response_p1(1, BestRows).

% To find the best response for Player 2 given Player 1 chooses strategy 2 (Row 2):
% best_response_p2(2, BestColumns).
