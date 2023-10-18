:- use_module(library(random)).
:- use_module(library(system)).

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