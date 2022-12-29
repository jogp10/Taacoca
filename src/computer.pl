:- ensure_loaded(board).

% Select randomly three pieces from the board
computer_easy_move(Board, Player, [(X1, Y1), (X2, Y2), (X3, Y3)], Direction) :-
    get_player_pieces(Board, Player, Pieces),
    length(Pieces, Count),
    (Count > 3 -> PiecesToPick = 3 ; PiecesToPick = Count),
    pick_piece(PiecesToPick, Pieces, [(X4, Y4), (X5, Y5), (X6, Y6)]),

    (Count == 1 -> X5 = X4, Y5 = Y4, X6 = X4, Y6 = Y4;
    Count == 2 -> X6 = X4, Y6 = Y4; true),

    offset(Y4, Offset1),
    offset(Y5, Offset2),
    offset(Y6, Offset3),
    X10 is X4 - Offset1,
    X11 is X5 - Offset2,
    X12 is X6 - Offset3,

    write('Computer selected pieces: '), write([(Y4, X10), (Y5, X11), (Y6, X12)]), nl,
    valid_moves(Board, [(X4, Y4), (X5, Y5), (X6, Y6)], ValidMoves),
    length(ValidMoves, Length),
    (Length > 0 -> (X1 = X4, Y1 = Y4, X2 = X5, Y2 = Y5, X3 = X6, Y3 = Y6, random_member(Direction, ValidMoves), write('Computer selected direction: '), write(Direction), nl)
    ; computer_easy_move(Board, Player, [(X1, Y1), (X2, Y2), (X3, Y3)], Direction)).


% Select randomly pieces from the player's pieces
pick_piece(0, _, _).
pick_piece(0, _, []).
pick_piece(Count, From, [X| Rest]) :-
    Count > 0,
    random_member(X, From),
    select(X, From, Remaining),
    NewCount is Count - 1,
    pick_piece(NewCount, Remaining, Rest).
