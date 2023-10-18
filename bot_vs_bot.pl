:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).
:- consult(bots_move).
:- consult(board_display).

play_game_Easy_vs_Easy(Board, Player, SoloPeace, Opponent,SoloOpponent):-
    (has_value_in_sublists(Player, Board) ->
        generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol),

        ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2; DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
            move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace)
         ;
            capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard)
        ),
        
        format('Type anything to see the Bot ~w move.',[Player]), read(Acess),
        display_board(NewBoard),nl,

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
            play_game_Easy_vs_Easy(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).

play_game_Easy_vs_Hard(Board, Player, SoloPeace, Opponent,SoloOpponent):-
    (has_value_in_sublists(Player, Board) ->
        ((Player = 'X/X')->
            generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol),
            ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2; DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
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
        
        format('Type anything to see the Bot ~w move.',[Player]), read(Acess),
        display_board(NewBoard),nl,

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
            play_game_Easy_vs_Hard(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).

play_game_Hard_vs_Easy(Board, Player, SoloPeace, Opponent,SoloOpponent):-
    (has_value_in_sublists(Player, Board) ->
        ((Player = 'O/O')->
            generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol),
            ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2; DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
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
        
        format('Type anything to see the Bot ~w move.',[Player]), read(Acess),
        display_board(NewBoard),nl,

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
            play_game_Hard_vs_Easy(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).

play_game_Hard_vs_Hard(Board, Player, SoloPeace, Opponent,SoloOpponent):-
    (has_value_in_sublists(Player, Board) ->
        generate_smart_move(Player, SoloPeace, Board, SoloOpponent,DeltaRow,DeltaCol,Play),
        ((DeltaRow=2,DeltaRow=2; DeltaRow=2, DeltaCol=0; DeltaRow=0, DeltaCol=2; DeltaRow = -2,DeltaRow = -2; DeltaRow = -2, DeltaCol=0; DeltaRow=0, DeltaCol = -2; DeltaRow = 2, DeltaCol = -2; DeltaRow= -2, DeltaCol = 2)->
            [ _, _, StartRow, StartCol, EndRow, EndCol]= Play,
            move(Board, StartRow, StartCol, EndRow, EndCol, NewBoard, Player, SoloPeace)
         ;
            [ _, _, StartRow, StartCol, EndRow, EndCol]= Play,
            capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard)
        ),
        
        format('Type anything to see the Bot ~w move.',[Player]), read(Acess),
        display_board(NewBoard),nl,

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
            play_game_Hard_vs_Hard(NewBoard, NextPlayer, NextSoloPeace, OtherOpponent,NextSoloOpponent))
    ;
        format('Player ~w has no valid moves left. Player ~w wins!', [Player, Opponent]), nl
    ).