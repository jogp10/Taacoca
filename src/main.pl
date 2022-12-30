:- ensure_loaded(board).
:- ensure_loaded(computer).
:- use_module(library(lists)).
:- use_module(library(random)).


% Display the main menu
play :-
    write('Welcome to the game!'), nl,
    write('Choose game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Exit'), nl,
    read(Option),
    ((Option == 1 -> play(p, p));
    (Option == 2 -> 
    write('Choose the difficulty:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty),
    choose_difficulty(Difficulty, C),
    play(p, C));
    (Option == 3 -> 
    write('Choose the difficulty for Computer 1:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty1),
    choose_difficulty(Difficulty1, C1),
    write('Choose the difficulty for Computer 2:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty2),
    choose_difficulty(Difficulty2, C2),
    play(C1, C2));
    (Option == 4 -> write('Bye!'), nl); play).


% Choose the computer difficulty
choose_difficulty(1, c1). % Easy
choose_difficulty(2, c2). % Hard


% Play the game
play(Player1, Player2) :-
    initial_state(Board),
    display_game(Board),
    play(Player1, Player2, Board, 1).

play(Player1, Player2, Board, Turn) :-
    write('Turn: '), write(Turn), nl,
    NewTurn is Turn + 1,

    % Player 1 turn
    (Player1 == c1 -> write('Computer 1 turn:'), nl, computer_easy_move(Board, 1, [(X1, Y1), (X2, Y2), (X3, Y3)], Direction);
    (Player1 == p -> write('Player 1 turn (Pieces: X):'), nl, pieces_to_move(Board, 1, (X1, Y1), (X2, Y2), (X3, Y3)), move_dir(Direction))),

    move(Board, [[(X1, Y1), (X2, Y2), (X3, Y3)], Direction], NewBoard),

    display_game(NewBoard),

    %check if the game is over
    game_over(NewBoard, Winner),
    (Winner == 1 -> (write('Player 1 wins!'), nl);
    (Winner == 2 -> (write('Player 2 wins!'), nl);(

    % Player 2 turn
    (Player2 == c1 -> write('Computer 2 turn:'), nl, computer_easy_move(NewBoard, 2, [(X4, Y4), (X5, Y5), (X6, Y6)], Direction2);
    (Player2 == p -> write('Player 2 turn (Pieces: O):'), nl, pieces_to_move(NewBoard, 2, (X4, Y4), (X5, Y5), (X6, Y6)), move_dir(Direction2))),

    move(NewBoard, [[(X4, Y4), (X5, Y5), (X6, Y6)], Direction2], EndTurnBoard),

    display_game(EndTurnBoard),

    %check if the game is over
    game_over(EndTurnBoard, Winner),
    (Winner == 1 -> (write('Player 1 wins!'), nl);
    (Winner == 2 -> (write('Player 2 wins!'), nl);

    play(Player1, Player2, EndTurnBoard, NewTurn)))))).
