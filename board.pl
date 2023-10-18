

% Define the game board as a 7x8 grid (7 rows and 8 columns)
board(7, 8).

% Define the initial positions of the pieces for each player.
% 'X/X' represents Player 1s pieces, and 'O/O' represents Player 2s pieces.
initial_board([
    ['X/X', 'X/X', 'X/X', 'X/X', 'X/X', 'X/X', 'X/X', 'X/X'],
    ['   ', '   ', '   ', '   ', '   ', '   ', '   ', '   '],
    ['   ', '   ', '   ', '   ', '   ', '   ', '   ', '   '],
    ['   ', '   ', '   ', '   ', '   ', '   ', '   ', '   '],
    ['   ', '   ', '   ', '   ', '   ', '   ', '   ', '   '],
    ['   ', '   ', '   ', '   ', '   ', '   ', '   ', '   '],
    ['O/O', 'O/O', 'O/O', 'O/O', 'O/O', 'O/O', 'O/O', 'O/O']
]).

% Predicate to display the board.
display_board(Board) :- nl,nl,nl,nl,nl,nl,nl,write('     1     2     3     4     5     6     7     8\n'),write('   -----------------------------------------------\n'),print_board(Board, 1),nl.

% Predicate to print the board.
print_board([], _).
print_board([Row|Rest], RowNum) :-
    write(RowNum), write(' | '), print_row(Row),write('\n'),write('   -----------------------------------------------\n'),
    NextRowNum is RowNum + 1,
    print_board(Rest, NextRowNum).

print_row([]).
print_row([Cell|Rest]) :-
    write(Cell), write(' | '),
    print_row(Rest).

% Predicate to select a piece at a specific row and column on the board.
select_piece(Board, Row, Col, Piece) :-
    nth1(Row, Board, BoardRow),  % Select the row using nth1/3
    nth1(Col, BoardRow, Piece).  % Select the element in the row using nth1/3

% Predicate to remove a piece at a specific row and column on the board.
remove_piece(Board, Row, Col, NewBoard) :-
    nth1(Row, Board, BoardRow),     % Select the row using nth1/3
    replace(BoardRow, Col, '   ', NewRow), % Replace the element in the row using replace/4
    replace(Board, Row, NewRow, NewBoard). % Replace the row in the board using replace/4

% Predicate to change the value of a cell at a specific row and column in the board.
change_cell(Board, Row, Col, NewValue, NewBoard) :-
    nth1(Row, Board, OldRow),        % Select the row using nth1/3
    replace(OldRow, Col, NewValue, ModifiedRow),  % Replace the element in the row using replace/4
    replace(Board, Row, ModifiedRow, NewBoard).  % Replace the row in the board using replace/4

% Helper predicate to replace an element in a list at a given position.
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

% Rule to check if a value is present in any sublist of a list of lists
has_value_in_sublists(_, []) :- fail.
has_value_in_sublists(Value, [Sublist | _]) :- has_value(Value, Sublist).
has_value_in_sublists(Value, [_ | Tail]) :- has_value_in_sublists(Value, Tail).

has_value(_, []) :- fail.
has_value(Value, [Value | _]).
has_value(Value, [_ | Tail]) :- has_value(Value, Tail).    