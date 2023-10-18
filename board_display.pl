:- use_module(library(lists)).

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