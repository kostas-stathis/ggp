% Define the game matrix here. Each cell is of the form cell(Row, Column, ValuePlayer1, ValuePlayer2).
% Example 2x2 game matrix:
cell(c, c, 2, 2).
cell(c, d, 0, 3).
cell(d, c, 3, 0).
cell(d, d, 1, 1).
%cell(d, d, 2, 2).
%cell(c, d, 1, 4).
%cell(d, c, 4, 1).
%cell(c, c, 3, 3).

% Best response for player 1
best_response_p1(Row, Col) :-
    cell(Row, Col, Value1, _),
    \+ ((cell(Row, _, OtherValue1, _), OtherValue1 > Value1)).

% Best response for player 2
best_response_p2(Row, Col) :-
    cell(_, Col, _, Value2),
    \+ ((cell(_, Col, _, OtherValue2), OtherValue2 > Value2)).

% Nash equilibrium occurs when both players are playing their best responses
nash_equilibrium(Row, Col) :-
    best_response_p1(Row, Col),
    best_response_p2(Row, Col).

% Finds all Nash equilibria in the game
find_nash_equilibria :-
    findall((Row, Col), nash_equilibrium(Row, Col), NashEquilibria),
    ( NashEquilibria = [] -> writeln('No Nash equilibrium found.');
      write('Nash equilibria at: '), writeln(NashEquilibria)
    ).
