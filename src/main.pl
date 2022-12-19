:- ensure_loaded(board).
:- use_module(library(lists)).

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
    initial_state(Board),
    play(Player1, Player2, Board, 1).

% Play the game

play(Player1, Player2, Board, Turn) :-
    display_game(Board),
    (Player1 == p -> write('Player 1 turn:'), nl, pieces_to_move((X1, Y1), (X2, Y2), (X3, Y3))),
    valid_moves(Board, [(X1, Y1), (X2, Y2), (X3, Y3)], ValidMoves),
    move_dir(Direction, ValidMoves),

    neighbor(X1, Y1, Direction, (A, B)),
    num_letter(B, B_LETTER), write(B_LETTER), write(A), nl,
    move_piece(Board, (Y1, X1), (B, A), NewBoard),

    neighbor(X2, Y2, Direction, (C, D)),
    num_letter(D, D_LETTER), write(D_LETTER), write(C), nl,
    move_piece(NewBoard, (Y2, X2), (D, C), NewBoard2),

    neighbor(X3, Y3, Direction, (E, F)),
    num_letter(F, F_LETTER), write(F_LETTER), write(E), nl,
    move_piece(NewBoard2, (Y3, X3), (F, E), NewBoard3),

    % check if the game is over
    %(check_player2_pieces(NewBoard3) -> write('Player 1 wins!'), nl, !);
    %(Player1 == c -> write('Computer 1 turn:'), nl, computerMove(Board, X1, Y1, X2, Y2)),

   

    %(Player2 == p -> write('Player 2 turn:'), nl, read(X1), read(Y1), read(X2), read(Y2)),
    % (Player2 == c -> write('Computer 2 turn:'), nl, computerMove(NewBoard, X1, Y1, X2, Y2)),
    % move(NewBoard, X1, Y1, X2, Y2, NewBoard2, Player2),
    NewTurn is Turn + 1,
    play(Player1, Player2, NewBoard3, NewTurn).

