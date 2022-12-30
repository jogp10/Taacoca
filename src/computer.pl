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


% Computer greedy algorithm
computer_hard_move(Board, Player, Pieces, Direction) :-
    valid_moves(Board, Player, ValidMoves),

    % calculate the value of the board after each move
    value_moves(Board, ValidMoves, Player, ValueMoves),

    % Find the maximum value
    maplist(first_element, ValueMoves, Values),

    % Find the element with the maximum value
    max_member(MaxValue, Values),
    max_member([MaxValue, Pieces, Direction], ValueMoves).
    write('Computer selected pieces: '), write(Pieces), nl, write('Computer selected direction: '), write(Direction), nl.


% Calculate the value of the board after each move
value_moves(_, [], _, []).
value_moves(Board, [[Pieces, Direction]| Rest], Player, [[ValueMove, Pieces, Direction]| RestValueMoves]) :-
    value_moves(Board, Rest, Player, RestValueMoves),

    move(Board, [Pieces, Direction], NewBoard),
    value(NewBoard, Player, ValueMove).
    

% Get the better move value from list of moves
better_value_move([], [-100, _, _]) :- write('No better value'), nl.
better_value_move([[ValueMove, Pieces, Direction] | Rest], [Value, BestPieces, BestDir]) :-
    better_value_move(Rest, [OldValue, OldPieces, OldDir]),
    (ValueMove > OldValue -> (write('Better value'), nl, BestPieces = OldPieces, BestDir = OldDir, Value = OldValue); true).


first_element([Number, _, _], Number).