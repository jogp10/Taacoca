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
    (Player1 == p -> write('Player 1 turn (Pieces: X):'), nl, pieces_to_move((X1, Y1), (X2, Y2), (X3, Y3)),
    valid_moves(Board, [(X1, Y1), (X2, Y2), (X3, Y3)], ValidMoves),
    move_dir(Direction, ValidMoves),

    neighbor(X1, Y1, Direction, (A, B)),
    move_piece(Board, (Y1, X1), (B, A), NewBoard),

    neighbor(X2, Y2, Direction, (C, D)),
    move_piece(NewBoard, (Y2, X2), (D, C), NewBoard2),

    neighbor(X3, Y3, Direction, (E, F)),
    move_piece(NewBoard2, (Y3, X3), (F, E), NewBoard3)),

    %check if the game is over
    ((check_player2_pieces(NewBoard3) -> write('Player 1 wins!'));
    (check_player1_in_last_row(NewBoard3) -> write('Player 1 wins!'));
    true
    ),

    display_game(NewBoard3),
    %(Player1 == c -> write('Computer 1 turn:'), nl, computerMove(Board, X1, Y1, X2, Y2)),

    (Player2 == p -> write('Player 2 turn (Pieces: O):'), nl, pieces_to_move((X4, Y4), (X5, Y5), (X6, Y6)),
    valid_moves(NewBoard3, [(X4, Y4), (X5, Y5), (X6, Y6)], ValidMoves2),
    move_dir(Direction2, ValidMoves2),

    neighbor(X4, Y4, Direction2, (A2, B2)),
    move_piece(NewBoard3, (Y4, X4), (B2, A2), NewBoard4),

    neighbor(X5, Y5, Direction2, (C2, D2)),
    move_piece(NewBoard4, (Y5, X5), (D2, C2), NewBoard5),

    neighbor(X6, Y6, Direction2, (E2, F2)),
    move_piece(NewBoard5, (Y6, X6), (F2, E2), NewBoard6)),

    %check if the game is over
    /*((check_player1_pieces(NewBoard6) -> write('Player 2 wins!'));
    (check_player2_in_last_row(NewBoard6) -> write('Player 2 wins!'));
    true
    ),*/
   


  
    % (Player2 == c -> write('Computer 2 turn:'), nl, computerMove(NewBoard, X1, Y1, X2, Y2)),
    % move(NewBoard, X1, Y1, X2, Y2, NewBoard2, Player2),
    NewTurn is Turn + 1,
    play(Player1, Player2, NewBoard6, NewTurn).

