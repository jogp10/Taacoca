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
    ((Option == 1 -> play(p, p));
    (Option == 2 -> 
    write('Choose the difficulty:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty),
    choose_difficulty(Difficulty, c),
    play(p, c));
    (Option == 3 -> 
    write('Choose the difficulty for Computer 1:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty1),
    choose_difficulty(Difficulty1, c1),
    write('Choose the difficulty for Computer 2:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty2),
    choose_difficulty(Difficulty2, c2),
    play(c1, c2));
    (Option == 4 -> write('Bye!'), nl)).

% Choose the computer difficulty

choose_difficulty(1, c1). % Easy
choose_difficulty(2, c2). % Hard


% Play the game

play(Player1, Player2) :-
    initial_state(Board),
    display_game(Board),
    play(Player1, Player2, Board, 1).

% Play the game

play(Player1, Player2, Board, Turn) :-

    %(Player1 == c -> write('Computer 1 turn:'), nl, computerMove(Board, X1, Y1, X2, Y2)),
    (Player1 == p -> write('Player 1 turn (Pieces: X):'), nl, pieces_to_move(Board, 1, (X1, Y1), (X2, Y2), (X3, Y3)),
    valid_moves(Board, [(X1, Y1), (X2, Y2), (X3, Y3)], ValidMoves),
    move_dir(Direction, ValidMoves),
    count_pieces(Board, 1, Count),

    neighbor(X1, Y1, Direction, (A, B)),
    neighbor(X2, Y2, Direction, (C, D)),
    neighbor(X3, Y3, Direction, (E, F)),

    (Count >= 1 -> move_piece(Board, (Y1, X1), (B, A), NewBoard);true),
    (Count >= 2 -> move_piece(NewBoard, (Y2, X2), (D, C), NewBoard2);true),
    (Count >= 3 -> move_piece(NewBoard2, (Y3, X3), (F, E), NewBoard4);true)),

    ((Count == 1, NewBoard3 = NewBoard);
    (Count == 2, NewBoard3 = NewBoard2);
    (Count >= 3, NewBoard3 = NewBoard4)),

    display_game(NewBoard3),

    %check if the game is over
    ((count_pieces(NewBoard3, 2, Count_Win), Count_Win == 0 -> write('Player 1 wins!'), nl, halt);
    (check_player_in_win_row(1, NewBoard3) -> write('Player 1 wins!'), nl, halt);true),


    % (Player2 == c -> write('Computer 2 turn:'), nl, computerMove(NewBoard, X1, Y1, X2, Y2)),
    (Player2 == p -> write('Player 2 turn (Pieces: O):'), nl, pieces_to_move(Board, 2, (X4, Y4), (X5, Y5), (X6, Y6)),
    valid_moves(NewBoard3, [(X4, Y4), (X5, Y5), (X6, Y6)], ValidMoves2),
    move_dir(Direction2, ValidMoves2),
    count_pieces(NewBoard3, 2, Count2),

    neighbor(X4, Y4, Direction2, (A2, B2)),
    (Count2 >= 1, move_piece(NewBoard3, (Y4, X4), (B2, A2), NewBoard4)),

    neighbor(X5, Y5, Direction2, (C2, D2)),
    (Count2 >= 2, move_piece(NewBoard4, (Y5, X5), (D2, C2), NewBoard5)),

    neighbor(X6, Y6, Direction2, (E2, F2)),
    (Count2 >= 3, move_piece(NewBoard5, (Y6, X6), (F2, E2), NewBoard7))),

    ((Count == 1, NewBoard6 = NewBoard4);
    (Count == 2, NewBoard6 = NewBoard5);
    (Count >= 3, NewBoard6 = NewBoard7)),

    display_game(NewBoard6),

    %check if the game is over
    ((count_pieces(NewBoard6, 1, Count_Win_), Count_Win_ == 0-> write('Player 2 wins!'), nl, halt);
    (check_player_in_win_row(2, NewBoard6) -> write('Player 2 wins!'), nl, halt);true),

    % move(NewBoard, X1, Y1, X2, Y2, NewBoard2, Player2),
    write('Turn: '), write(Turn), nl,
    NewTurn is Turn + 1,
    play(Player1, Player2, NewBoard6, NewTurn).
