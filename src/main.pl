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

    (Player1 == c1 -> write('Computer 1 turn:'), nl, computer_easy_move(Board, 1, [(X1, Y1), (X2, Y2), (X3, Y3)], Direction);
    (Player1 == p -> write('Player 1 turn (Pieces: X):'), nl, pieces_to_move(Board, 1, (X1, Y1), (X2, Y2), (X3, Y3)),
    valid_moves(Board, [(X1, Y1), (X2, Y2), (X3, Y3)], ValidMoves),
    move_dir(Direction, ValidMoves))),
    count_pieces(Board, 1, Count),

    (Count >= 1 -> neighbor(X1, Y1, Direction, (A, B));true),
    (Count >= 2 -> neighbor(X2, Y2, Direction, (C, D)); true),
    (Count >= 3 -> neighbor(X3, Y3, Direction, (E, F)); true),

    (Count >= 1 -> move_piece(Board, (Y1, X1), (B, A), NewBoard);true),
    (Count >= 2 -> move_piece(NewBoard, (Y2, X2), (D, C), NewBoard2);true),
    (Count >= 3 -> move_piece(NewBoard2, (Y3, X3), (F, E), NewBoard4);true),

    ((Count == 1, NewBoard3 = NewBoard);
    (Count == 2, NewBoard3 = NewBoard2);
    (Count >= 3, NewBoard3 = NewBoard4)),

    display_game(NewBoard3),

    %check if the game is over
    count_pieces(NewBoard3, 2, Count_Win), 
    (Count_Win == 0 -> (write('Player 1 wins!'), nl);
    (check_player_in_win_row(1, NewBoard3) -> (write('Player 1 wins!'), nl);(

    (Player2 == c1 -> write('Computer 2 turn:'), nl, computer_easy_move(Board, 2, [(X4, Y4), (X5, Y5), (X6, Y6)], Direction2);
    (Player2 == p -> write('Player 2 turn (Pieces: O):'), nl, pieces_to_move(Board, 2, (X4, Y4), (X5, Y5), (X6, Y6)),
    valid_moves(NewBoard3, [(X4, Y4), (X5, Y5), (X6, Y6)], ValidMoves2),
    move_dir(Direction2, ValidMoves2))),
    count_pieces(NewBoard3, 2, Count2),

    (Count2 >= 1 -> neighbor(X4, Y4, Direction2, (A2, B2));true),
    (Count2 >= 2 -> neighbor(X5, Y5, Direction2, (C2, D2));true),
    (Count2 >= 3 -> neighbor(X6, Y6, Direction2, (E2, F2));true),

    (Count2 >= 1 -> move_piece(NewBoard3, (Y4, X4), (B2, A2), NewBoard5);true),
    (Count2 >= 2 -> move_piece(NewBoard5, (Y5, X5), (D2, C2), NewBoard7);true),
    (Count2 >= 3 -> move_piece(NewBoard7, (Y6, X6), (F2, E2), NewBoard8);true),

    ((Count == 1, NewBoard6 = NewBoard5);
    (Count == 2, NewBoard6 = NewBoard7);
    (Count >= 3, NewBoard6 = NewBoard8)),

    display_game(NewBoard6),

    %check if the game is over
    count_pieces(NewBoard6, 1, Count_Win_), 
    (Count_Win_ == 0 -> (write('Player 2 wins!'), nl);
    (check_player_in_win_row(2, NewBoard6) -> write('Player 2 wins!'), nl);(

    % move(NewBoard, X1, Y1, X2, Y2, NewBoard2, Player2),
    write('Turn: '), write(Turn), nl,
    NewTurn is Turn + 1,
    play(Player1, Player2, NewBoard6, NewTurn)))))).
