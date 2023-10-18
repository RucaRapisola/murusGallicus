:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).

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

set_new_random_seed :-
    now(Time),
    Seed is Time mod 30269,
    setrand(Seed).

% Entry point for the game
:- initialization(main).
main :-
    nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
    nl,nl,
    set_new_random_seed, % Ensure that the random bot does not play the same initial moves all games.
    repeat,  % This will repeat until a valid GameMode is entered.
    printMainMenu,
    read(GameMode),
    (checkInput(GameMode) -> true ; write('Invalid option!'), nl, fail).

checkInput(1) :-
    initial_board(Board),
    display_board(Board),
    play_game(Board, 'X/X', ' X ', 'O/O', ' O ').

checkInput(2) :-
    printChooseLevelBot, read(BotLevel),
    ((BotLevel = 1) ->
        printChooseYourPiece,read(PlayerControl),
        initial_board(Board),
        display_board(Board),
        ((PlayerControl = 1)->
            play_game_against_Easy_bot(Board, 'X/X', ' X ', 'O/O', ' O ','X/X')
        ;
            play_game_against_Easy_bot(Board, 'X/X', ' X ', '0/O', ' O ','O/O')
        )
    ; 
    (BotLevel = 2)->
        printChooseYourPiece,read(PlayerControl),
        initial_board(Board),
        display_board(Board),
        ((PlayerControl = 1)->
            play_game_against_Hard_bot(Board, 'X/X', ' X ', 'O/O', ' O ','X/X')
        ;
            play_game_against_Hard_bot(Board, 'X/X', ' X ', '0/O', ' O ','O/O')
        )
    ;
    write('Invalid option!'), nl,checkInput(2)).

checkInput(3):-
    printBotVSBot, read(BotsGame),
    bot_Game(BotsGame).

bot_Game(1):-
    initial_board(Board),
    display_board(Board),
    play_game_Easy_vs_Hard(Board, 'X/X', ' X ', 'O/O', ' O ').

bot_Game(2):-
    initial_board(Board),
    display_board(Board),
    play_game_Hard_vs_Easy(Board, 'X/X', ' X ', 'O/O', ' O ').

bot_Game(3):-
    initial_board(Board),
    display_board(Board),
    play_game_Easy_vs_Easy(Board, 'X/X', ' X ', 'O/O', ' O ').

bot_Game(4):-
    initial_board(Board),
    display_board(Board),
    play_game_Hard_vs_Hard(Board, 'X/X', ' X ', 'O/O', ' O ').

bot_Game(5):-
    write('Invalid option!'),nl,
    printBotVSBot,
    read(Input),
    bot_Game(Input).          

checkInput(0):-
    write('Exiting...').

checkInput(_):-
    write('Invalid option!'),nl,
    printMainMenu,
    read(Input),
    checkInput(Input).

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



% Predicate to get a players move input
get_move(Player,SoloPeace, StartRow, StartCol, EndRow, EndCol,Board,DeltaRow,DeltaCol,SoloOpponent) :-
    repeat,
    format('Player ~w, enter your move (start row, start col, end row, end col): ', [Player]),nl,
    write('Start row: '), read(StartRow),nl,
    write('Start Col'),read(StartCol),nl,
    write('End row'),read(EndRow),nl,
    write('End Col'),read(EndCol),nl,
    (valid_move(Player,SoloPeace, StartRow, StartCol, EndRow, EndCol,Board,DeltaRow,DeltaCol,SoloOpponent) -> true ; write('Invalid move, try again.'),nl, fail).

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

% Helper predicate to replace an element in a list at a given position.
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).

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
    DeltaRow is EndRow - StartRow,
    DeltaCol is EndCol - StartCol,
    
    % Ensure that the move is either orthogonal or diagonal (DeltaRow and DeltaCol are 0, 1, or 2).
    ((DeltaRow =:= 0, DeltaCol =:= 2 ;  % Orthogonal
     DeltaRow =:= 2, DeltaCol =:= 0 ;  % Orthogonal
     DeltaRow =:= 2, DeltaCol =:= 2;
     DeltaRow =:= 2, DeltaCol =:= -2;
     DeltaRow =:= -2, DeltaCol =:= 2;
     DeltaRow =:= 0, DeltaCol =:= -2 ;  % Orthogonal
     DeltaRow =:= -2, DeltaCol =:= 0 ;  % Orthogonal
     DeltaRow =:= -2, DeltaCol =:= -2) -> 
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
      DeltaRow =:= 1, DeltaCol =:= 1;
      DeltaRow =:= 1, DeltaCol =:= -1;
      DeltaRow =:= -1, DeltaCol =:= 1;
      DeltaRow =:= 0, DeltaCol =:= -1;  % Orthogonal
      DeltaRow =:= -1, DeltaCol =:= 0;  % Orthogonal
      DeltaRow =:= -1, DeltaCol =:= -1) ->
        select_piece(Board,EndRow,EndCol,DestinationPiece),
        (DestinationPiece = SoloOpponent)  
    ).


% Rule to check if a value is present in any sublist of a list of lists
has_value_in_sublists(_, []) :- fail.
has_value_in_sublists(Value, [Sublist | _]) :- has_value(Value, Sublist).
has_value_in_sublists(Value, [_ | Tail]) :- has_value_in_sublists(Value, Tail).

has_value(_, []) :- fail.
has_value(Value, [Value | _]).
has_value(Value, [_ | Tail]) :- has_value(Value, Tail).

printMainMenu:-
    write(' _________________________________________________________________________'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|                              Murus Gallicus                             |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|  Choose the type of the game:                                           |'),nl,
    write('|                                                                         |'),nl,
    write('|      1- Player vs Player                                                |'),nl,
    write('|      2- Player vs Bot                                                   |'),nl,
    write('|      3- Bot vs Bot                                                      |'),nl,
    write('|      0- Exit                                                            |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|_________________________________________________________________________|'),nl,nl.


capture_piece(Board,Player,SoloPeace,StartRow,StartCol,EndRow,EndCol,NewBoard):-
    remove_piece(Board, StartRow, StartCol, TempBoard),

    % Check if the middle position is empty or if contains a SoloPeace.
    select_piece(TempBoard, StartRow, StartCol, DestinationPiece),
    (DestinationPiece = '   ' -> change_cell(TempBoard,StartRow,StartCol,SoloPeace,IntermediateBoard)),

    remove_piece(IntermediateBoard, EndRow, EndCol, NewBoard).


% Generate a random valid move with a maximum delta of 2 in both row and column
%generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol) :-
%    repeat,
%    random(1,8, StartRow), % Generates a random integer between 0 and 7
%    random(1,9, StartCol), % Generates a random integer between 0 and 8
%    random(0,5, DeltaRowSec), % Generates a random integer between 0 and 4
%    random(0,5, DeltaColSec), % Generates a random integer between 0 and 4
%    EndRow is StartRow + DeltaRowSec - 2, % Adjust to be within 1 to 7
%    EndCol is StartCol + DeltaColSec - 2, % Adjust to be within 1 to 8
%    (valid_move(Player, SoloPeace, StartRow, StartCol, EndRow, EndCol, Board, DeltaRow, DeltaCol, SoloOpponent) ; fail). %generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol)

generate_random_valid_move(Player, SoloPeace, Board, DeltaRow, DeltaCol, SoloOpponent, StartRow, StartCol, EndRow, EndCol):-

    create_all_valid_moves(Player,SoloPeace,Board,SoloOpponent,Moves),
    length(Moves,N),
    random(1,N,X),
    get_random_move(X,Moves,[DeltaRow, DeltaCol, StartRow, StartCol, EndRow, EndCol]).


get_random_move(1,[Play| _],Play).
get_random_move(N,[ _| T],P):- N1 is N-1,
    get_random_move(N1,T,P).

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


printChooseLevelBot:-
    write(' _________________________________________________________________________'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|                              Murus Gallicus                             |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|  Choose the level of the bot:                                           |'),nl,
    write('|                                                                         |'),nl,
    write('|      1- Easy mode                                                       |'),nl,
    write('|      2- Hard mode                                                       |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|_________________________________________________________________________|'),nl,nl.

printChooseYourPiece:-
    write(' _________________________________________________________________________'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|                              Murus Gallicus                             |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|  Choose the pieces that you wanna control:                              |'),nl,
    write('|                                                                         |'),nl,
    write('|      1- X/X Pieces                                                       |'),nl,
    write('|      2- O/O Pieces                                                      |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|_________________________________________________________________________|'),nl,nl.    

printBotVSBot:-
    write(' _________________________________________________________________________'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|                              Murus Gallicus                             |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|  Choose the level of the two bots that will face each other:            |'),nl,
    write('|                                                                         |'),nl,
    write('|      1- Bot1(Easy) vs Bot2(Hard)                                        |'),nl,
    write('|      2- Bot1(Hard) vs Bot2(Easy)                                        |'),nl,
    write('|      3- Bot1(Easy) vs Bot2(Easy)                                        |'),nl,
    write('|      4- Bot1(Hard) vs Bot2(Hard)                                        |'),nl,
    write('|                                                                         |'),nl,
    write('|                                                                         |'),nl,
    write('|_________________________________________________________________________|'),nl,nl.

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
