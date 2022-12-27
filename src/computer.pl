:- use_module(library(random)).
:- ensure_loaded(board).

% Select randomly three pieces from the board
computer_easy_move(Board, Player, (X1, Y1), (X2, Y2), (X3, Y3)) :- 
    get_player_pieces(Board, Player, Pieces),
    pick_piece(3, Pieces, [(X1, Y1), (X2, Y2), (X3, Y3)]).

pick_piece(0, _, []).
pick_piece(Count, From, [X| Rest]) :-
    Count > 0,
    random_member(X, From),
    select(X, From, Remaining),
    NewCount is Count - 1,
    pick_piece(NewCount, Remaining, Rest).
