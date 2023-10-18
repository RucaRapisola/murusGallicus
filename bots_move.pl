:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).

generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol):-

    create_all_valid_moves(Player,SoloPeace,Board,SoloOpponent,Moves),
    length(Moves,N),
    random(1,N,X),
    get_random_move(X,Moves,[DeltaRow, DeltaCol, StartRow, StartCol, EndRow, EndCol]).

get_random_move(1,[Play| _],Play).
get_random_move(N,[ _| T],P):- N1 is N-1,
    get_random_move(N1,T,P).


% Generate a smart move for the bot
generate_smart_move(Player, SoloPeace, Board, SoloOpponent,DeltaRow,DeltaCol,Play) :-

    create_all_valid_moves(Player,SoloPeace,Board,SoloOpponent,Moves),

    execute_valid_move(Player, SoloPeace, Board, Moves, Play),

    [DeltaRow_, DeltaCol_, StartRow, StartCol, EndRow, EndCol]= Play,

    DeltaRow is abs(DeltaRow_),
    DeltaCol is abs(DeltaCol_).

execute_valid_move(Player, SoloPeace, Board, Moves, Play):-
   select_reach_first_row_move(Player, SoloPeace, Board, Moves, Play).

execute_valid_move(Player, SoloPeace, Board, Moves, Play):-
   select_capture_opponent_move(Player, SoloPeace, Board, Moves, Play).

execute_valid_move(Player, SoloPeace, Board, Moves, Play):-
    generate_random_valid_move_smart(Player, SoloPeace, Board, Moves, Play).    

% Select a move to reach the opponents first row.
select_reach_first_row_move(_, _, _, [],Play) :- fail. % No moves to reach the final row, return false.

select_reach_first_row_move(Player, SoloPeace, Board, [Move | Rest],Play) :-
    % Extract the elements from the move list.
    [DeltaRow, DeltaCol, StartRow, StartCol, EndRow, EndCol] = Move,

    % Check if the move allows reaching the opponents first row.
    (Player = 'X/X', EndRow = 7,

    % Check if the move is a valid move and meets the first-row condition.
    valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board, _, _, SoloOpponent) -> (Play = Move)); select_reach_first_row_move(Player, SoloPeace, Board, Rest,Play).
    
select_reach_first_row_move(Player, SoloPeace, Board, [Move | Rest],Play) :-
    % Extract the elements from the move list.
    [DeltaRow, DeltaCol, StartRow, StartCol, EndRow, EndCol] = Move,

    % Check if the move allows reaching the opponents first row.
    (Player = 'O/O', EndRow = 1,

    % Check if the move is a valid move and meets the first-row condition.
    valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board, _, _, SoloOpponent) -> (Play = Move)); select_reach_first_row_move(Player, SoloPeace, Board, Rest,Play).
    


% Select a move to capture the opponents piece.
select_capture_opponent_move(_, _, _, [], Play) :- fail. % No captures left, return false.

select_capture_opponent_move(Player, SoloPeace, Board, [Move | Rest],Play):-
    % Extract the elements from the move list.
    [DeltaRow_, DeltaCol_, StartRow, StartCol, EndRow, EndCol] = Move,

    % Check if the move is a possible capture.
    ((DeltaRow_ = 1 , DeltaCol_ = 1 ; DeltaRow_ = 1, DeltaCol_ = 0; DeltaRow_ = 1, DeltaCol_ = -1; DeltaRow_ = 0, DeltaCol_ = 1; DeltaRow_ = 0, DeltaCol_ = -1; DeltaRow_ = -1, DeltaCol_ = 1; DeltaRow_= -1, DeltaCol_= 0; DeltaRow_ = -1, DeltaCol_ = -1),

    valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board, DeltaRow, DeltaCol, SoloOpponent)->(Play = Move)) ; select_capture_opponent_move(Player, SoloPeace, Board, Rest,Play).


% Generate a random valid move with a maximum delta of 2 in both row and column
generate_random_valid_move_smart(Player, SoloPeace, Board, Moves, Play) :-
    % Execute the first valid_move
    length(Moves,N),
    random(1,N,X),
    get_random_move(X,Moves,Play).


create_all_valid_moves(Player,SoloPeace,Board,SoloOpponent,Moves):-
    findall(
        [DeltaRow, DeltaCol, StartRow, StartCol, EndRow, EndCol],
        (
            between(1, 7, StartRow),   % Iterate through possible start rows
            between(1, 8, StartCol),   % Iterate through possible start columns
            between(-2,2, DeltaRow),  % Iterate through possible delta rows
            between(-2, 2, DeltaCol),  % Iterate through possible delta columns
            EndRow is StartRow + DeltaRow,
            EndCol is StartCol + DeltaCol,
            valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board, _, _, SoloOpponent)
        ),
        Moves
    ).
