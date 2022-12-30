:- ensure_loaded(board).

% Select randomly three pieces from the board
computer_easy_move(Board, Player, [(X1, Y1), (X2, Y2), (X3, Y3)], Direction) :-
    valid_moves(Board, Player, ValidMoves),
    random_member([[(X1, Y1), (X2, Y2), (X3, Y3)], Direction], ValidMoves), write('Computer selected pieces: '), write([(Y1, X1), (Y2, X2), (Y3, X3)]), nl, write('Computer selected direction: '), write(Direction), nl.


% Select randomly pieces from the player's pieces
pick_piece(0, _, _).
pick_piece(0, _, []).
pick_piece(Count, From, [X| Rest]) :-
    Count > 0,
    random_member(X, From),
    select(X, From, Remaining),
    NewCount is Count - 1,
    pick_piece(NewCount, Remaining, Rest).
