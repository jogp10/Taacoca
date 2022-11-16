
% Display the main menu

play :-
    write('Welcome to the game!'), nl,
    write('Choose game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    write('4. Exit'), nl,
    read(Option),
    (Option == 1 -> play(p, p)),
    (Option == 2 -> play(p, c)),
    (Option == 3 -> play(c, c)),
    (Option == 4 -> write('Bye!'), nl).


% Play the game

play(Player1, Player2) :-
    initBoard(Board),
    play(Player1, Player2, Board, 1).


% Play the game

play(Player1, Player2, Board, Turn) :-
    displayBoard(Board),
    (Player1 == p -> write('Player 1 turn:'), nl, read(X1), read(Y1), read(X2), read(Y2)),
    (Player1 == c -> write('Computer 1 turn:'), nl, computerMove(Board, X1, Y1, X2, Y2)),
    move(Board, X1, Y1, X2, Y2, NewBoard, Player1),
    (Player2 == p -> write('Player 2 turn:'), nl, read(X1), read(Y1), read(X2), read(Y2)),
    (Player2 == c -> write('Computer 2 turn:'), nl, computerMove(NewBoard, X1, Y1, X2, Y2)),
    move(NewBoard, X1, Y1, X2, Y2, NewBoard2, Player2),
    NewTurn is Turn + 1,
    play(Player1, Player2, NewBoard2, NewTurn).

