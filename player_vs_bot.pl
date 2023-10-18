:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).
:- consult(rules_about_move).
:- consult(bots_move).
:- consult(board_display).

play_game_against_Easy_bot(Board, Player, SoloPeace, Opponent,SoloOpponent,PlayerControl) :-
    (has_value_in_sublists(Player, Board) ->
        ((Player = PlayerControl)->
            get_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board,DeltaRow,DeltaCol,SoloOpponent)
        ;
            generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol)
        ),

        ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2; DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
            move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace)
         ;
            capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard)
        ),
        
        ((Player = PlayerControl)->
            display_board(NewBoard),nl
            ;
            write('Type anything to see the Bots move.'), read(Acess),
            display_board(NewBoard),nl
        ),

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
            play_game_against_Easy_bot(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent,PlayerControl))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).

play_game_against_Hard_bot(Board, Player, SoloPeace, Opponent,SoloOpponent,PlayerControl) :-
    (has_value_in_sublists(Player, Board) ->
        ((Player = PlayerControl)->
            get_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board,DeltaRow,DeltaCol,SoloOpponent),
            ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2;DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
                move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace)
            ;
                capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard)
            )
        ;
            generate_smart_move(Player, SoloPeace, Board, SoloOpponent,DeltaRow,DeltaCol,Play),
            ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2; DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
                [ _, _, StartRow, StartCol, EndRow, EndCol]= Play,
                move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace)
            ;
                [ _, _, StartRow, StartCol, EndRow, EndCol]= Play,
                capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard)
            )
        ),
        
        ((Player = PlayerControl)->
            display_board(NewBoard),nl
            ;
            write('Type anything to see the Bots move.'), read(Acess),
            display_board(NewBoard),nl
        ),

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
            play_game_against_Hard_bot(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent,PlayerControl))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).