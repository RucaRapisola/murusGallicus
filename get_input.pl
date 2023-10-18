:- use_module(library(lists)).
:- use_module(library(random)).

% Predicate to get a players move input
get_move(Player,SoloPeace, StartRow, StartCol, EndRow, EndCol,Board,DeltaRow,DeltaCol,SoloOpponent) :-
    repeat,
    format('Player ~w, enter your move (start row, start col, end row, end col): ', [Player]),nl,
    write('Start row: '), read(StartRow),nl,
    write('Start Col'),read(StartCol),nl,
    write('End row'),read(EndRow),nl,
    write('End Col'),read(EndCol),nl,
    (valid_move(Player,SoloPeace, StartRow, StartCol, EndRow, EndCol,Board,DeltaRow,DeltaCol,SoloOpponent) -> true ; write('Invalid move, try again.'),nl, fail).

% Generate a random valid move with a maximum delta of 2 in both row and column
generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol) :-
    repeat,
    random(1,8, StartRow), % Generates a random integer between 0 and 7
    random(1,9, StartCol), % Generates a random integer between 0 and 8
    random(0,5, DeltaRowSec), % Generates a random integer between 0 and 4
    random(0,5, DeltaColSec), % Generates a random integer between 0 and 4
    EndRow is StartRow + DeltaRowSec - 2, % Adjust to be within 1 to 7
    EndCol is StartCol + DeltaColSec - 2, % Adjust to be within 1 to 8
    (valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board, DeltaRow, DeltaCol, SoloOpponent) -> true ; fail).