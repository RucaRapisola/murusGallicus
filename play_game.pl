:- use_module(library(lists)).
:- consult(board).
:- consult(get_input).


play_game(Board, Player, SoloPeace, Opponent,SoloOpponent) :-
    (has_value_in_sublists(Player, Board) ->
        get_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board,DeltaRow,DeltaCol,SoloOpponent),
        ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2)->
            move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace)
         ;
            capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard)
        ),

        display_board(NewBoard),

        % Check if the player has reached the first row.
        ((Player = 'O/O', EndRow = 1 ; Player = 'X/X', EndRow = 7) ->
            format('Player controlling the ~w pieces wins by reaching the opponent´s first row!', [Player]), nl
        ;
            % Check for win/draw conditions and update Player based on game rules.

            % Switch to the other player for the next turn.
            (Player = 'X/X' -> NextPlayer = 'O/O' ; NextPlayer = 'X/X'),
            (SoloPeace = ' X ' -> NextSoloPeace = ' O ' ; NextSoloPeace = ' X '),
            (Opponent = 'O/O' -> OtherOpponent = 'X/X' ; OtherOpponent = 'O/O'),
            (SoloOpponent = ' O ' -> NextSoloOpponent = ' X '; NextSoloOpponent = ' O '),

            % Continue the game with the other player.
            play_game(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).

% Predicate to make a move. It will replace the piece in the start position with an empty cell and place the piece in the middle and in the end position.
move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace) :-
    % Remove the Piece of the Initial Square.
    /*select_piece(Board, StartRow, StartCol, Piece),*/
    remove_piece(Board, StartRow, StartCol, TempBoard),
    
    % Calculate the middle row and column.
    MiddleRow is (StartRow + EndRow) // 2,
    MiddleCol is (StartCol + EndCol) // 2,
    
    % Check if the middle position is empty or if contains a SoloPeace.
    select_piece(Board, MiddleRow, MiddleCol, DestinationPiece),
    (DestinationPiece = '   ' -> change_cell(TempBoard,MiddleRow,MiddleCol,SoloPeace,IntermediateBoard);
        DestinationPiece = SoloPeace -> change_cell(TempBoard, MiddleRow, MiddleCol, Player, IntermediateBoard)
    ),

    % Check if the destination position is empty or if contains a SoloPeace.
    select_piece(IntermediateBoard, EndRow, EndCol, DestinationPiece2),
    (DestinationPiece2 = '   ' -> change_cell(IntermediateBoard, EndRow, EndCol, SoloPeace, NewBoard);
        DestinationPiece2 = SoloPeace-> change_cell(IntermediateBoard, EndRow, EndCol, Player, NewBoard)
    ).

% Rules of the game to make sure the move that player inserted is valid.
valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol,Board,DeltaRow,DeltaCol,SoloOpponent) :-
    % Check if the move is within bounds.
    (StartRow >= 1, StartRow =< 7),
    (StartCol >= 1, StartCol =< 8),
    (EndRow >= 1, EndRow =< 7),
    (EndCol >= 1, EndCol =< 8),

    % Check if the starting square has a piece of the player ('X/X' or 'O/O' depending on the player who is moving).
    (select_piece(Board, StartRow, StartCol, InitialPiece),
    InitialPiece = Player),

    % Calculate the absolute row and column differences between start and end positions.
    DeltaRow is abs(EndRow - StartRow),
    DeltaCol is abs(EndCol - StartCol),
    
    % Ensure that the move is either orthogonal or diagonal (DeltaRow and DeltaCol are 0, 1, or 2).
    ((DeltaRow =:= 0, DeltaCol =:= 2 ;  % Orthogonal
     DeltaRow =:= 2, DeltaCol =:= 0 ;  % Orthogonal
     DeltaRow =:= 2, DeltaCol =:= 2) -> 
        % Calculate the middle row and column.
        MiddleRow is (StartRow + EndRow) // 2,
        MiddleCol is (StartCol + EndCol) // 2,

        % Check if the destination position is empty or if contains a SoloPeace.
        select_piece(Board, EndRow, EndCol, DestinationPiece),
        (DestinationPiece = '   '; DestinationPiece = SoloPeace),

        % Check if the intermedium square is empty or contains a SoloPeace.
        ((DeltaRow =:= 2, DeltaCol =:= 2 ; DeltaRow =:= -2, DeltaCol =:= -2 ; DeltaRow =:= 2, DeltaCol =:= -2 ; DeltaRow =:= -2, DeltaCol =:= 2;DeltaRow =:= 2, DeltaCol =:= 0 ; DeltaRow =:= 0, DeltaCol =:= 2 ; DeltaRow =:= -2, DeltaCol =:= 0 ; DeltaRow =:= 0, DeltaCol =:= -2),
            select_piece(Board, MiddleRow, MiddleCol, MiddlePiece),
            (MiddlePiece = '   ' ; MiddlePiece = SoloPeace))
    ;
     (DeltaRow =:= 0, DeltaCol =:= 1;  % Orthogonal
      DeltaRow =:= 1, DeltaCol =:= 0;  % Orthogonal
      DeltaRow =:= 1, DeltaCol =:= 1) ->
        select_piece(Board,EndRow,EndCol,DestinationPiece),
        (DestinationPiece = SoloOpponent)  
    ).

capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard):-
    remove_piece(Board, StartRow, StartCol, TempBoard),

    % Check if the middle position is empty or if contains a SoloPeace.
    select_piece(TempBoard, StartRow, StartCol, DestinationPiece),
    (DestinationPiece = '   ' -> change_cell(TempBoard,StartRow,StartCol,SoloPeace,IntermediateBoard)),

    remove_piece(IntermediateBoard, EndRow, EndCol, NewBoard).            