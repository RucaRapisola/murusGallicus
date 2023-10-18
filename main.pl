:- use_module(library(lists)).
:- consult(board).
:- consult(play_against_bot).
:- consult(play_game).
:- consult(display_screen).
:- consult(play_bot_bot).

% Entry point for the game
:- initialization(main).
main :-
    nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
    nl,nl,
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
    (BotLevel = 1 ->
        printChooseYourPiece,read(PlayerControl),
        initial_board(Board),
        display_board(Board),
        ((PlayerControl = 1)->
            play_game_against_bot(Board, 'X/X', ' X ', 'O/O', ' O ','X/X')
        ;
            play_game_against_bot(Board, 'X/X', ' X ', '0/O', ' O ','O/O')
        )
    ; write('Invalid option!'), nl, fail).

checkInput(3):-
    printBotVSBot, read(BotsGame),
    initial_board(Board),
    display_board(Board),
    play_game_Easy_vs_Easy(Board,'X/X', ' X ', 'O/O', ' O ' ).


checkInput(0):-
    write('Exiting...').

checkInput(_):-
    write('Invalid option!'),nl,
    printMainMenu,
    read(Input),
    checkInput(Input).    