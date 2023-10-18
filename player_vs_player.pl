:- use_module(library(lists)).
:- use_module(library(system)).
:- consult(rules_about_move).
:- consult(board_display).

play_game(Board, Player, SoloPeace, Opponent,SoloOpponent) :-
    (has_value_in_sublists(Player, Board) ->
        get_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board,DeltaRow,DeltaCol,SoloOpponent),
        DeltaRow_ is abs(DeltaRow),
        DeltaCol_ is abs(DeltaCol),
        ((DeltaRow_=2,DeltaCol_=2; DeltaRow_=2, DeltaCol_=0; DeltaRow_=0, DeltaCol_=2)->
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