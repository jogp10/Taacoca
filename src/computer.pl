:- use_module(library(random)).
:- ensure_loaded(board).

% Select randomly three pieces from the board
computer_easy_move(Board, Player, (X1, Y1), (X2, Y2), (X3, Y3)) :- 
    get_player_pieces(Board, Player, Pieces),
    random_member((X1, Y1), Pieces),
    random_member((X2, Y2), Pieces),
    random_member((X3, Y3), Pieces).
