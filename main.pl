:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).
:- consult(menu_prints).
:- consult(player_vs_player).
:- consult(player_vs_bot).
:- consult(bot_vs_bot).
:- consult(board_display).

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
