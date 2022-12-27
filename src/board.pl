:- use_module(library(lists)).

% Define the directions of the board
directions([nw, ne, e, se, sw, w]).

% Define the players
players([1, 2]).
player(1, 'X').
player(2, 'O').

% offset for the board
offset(6, 1).
offset(7, 2).
offset(8, 3).
offset(9, 4).
offset(_, 0).

% Initiate the board
/* e.g.
  -1 - out of bounds
   0 - empty
   1 - player 1
   2 - player 2 
*/
initial_state(Board) :- Board = 
       [[0 , 0, 0, 0, 0,-1,-1,-1,-1],
        [0 , 2, 0, 0, 0, 0,-1,-1,-1],
        [0 , 0, 0, 1, 0, 0, 0,-1,-1],
        [0 , 0, 0, 0, 0, 0, 0, 0,-1],
        [0 , 0, 0, 0, 0, 0, 0, 0, 0],
        [-1, 0, 0, 0, 0, 0, 0, 0, 0],
        [-1,-1, 0, 0, 0, 2, 2, 0, 0],
        [-1,-1,-1, 0, 2, 2, 2, 2, 0],
        [-1,-1,-1,-1, 2, 2, 2, 2, 2]].
      



% Display the board
/* e.g.
    -     1 2 3 4
    I     - - - - - 5
    H    - - - - - - 6
    G   - - - - - - - 7
    F  - - - - - - - - 8
    E - - - - - - - - - 9
    D  - - - - - - - - 8
    C   - - - - - - - 7
    B    - - - - - - 6
    A     - - - - - 5
          1 2 3 4

*/
display_game(Board) :-
    write('       1 2 3 4 5'), nl,
    display_game(Board, 1).

display_game([], _).
display_game([H|T], R) :-
    num_letter(R, RowLetter),
    write(RowLetter), write(' '),
    spacing(R),
    last_num_cell(R, N),
    display_row(H, N),
    nl,
    NewR is R + 1,
    display_game(T, NewR).


% Display a row of the board
/* e.g.
    A     - - - - -
*/
display_row([], C) :-  write(' '), write(C),  nl.
display_row([H|T], C) :-
    ((H == -1 -> write(''));
    (H == 0 -> write('- '));
    (H == 1 -> write('X '));
    (H == 2 -> write('O '))),
    display_row(T, C).
 

% Add spacing/indexing to the board
spacing(1) :- write('     ').
spacing(2) :- write('    ').
spacing(3) :- write('   ').
spacing(4) :- write('  ').
spacing(5) :- write(' ').
spacing(9) :- write('     ').
spacing(8) :- write('    ').
spacing(7) :- write('   ').
spacing(6) :- write('  ').


% Convert Num to Letter and Letter to Num
/*
    num_letter(0, X) : X -> 'I'
    num_letter(X, 'B'): X -> 7
*/
num_letter(1, 'i').
num_letter(2, 'h').
num_letter(3, 'g').
num_letter(4, 'f').
num_letter(5, 'e'). 
num_letter(6, 'd').
num_letter(7, 'c'). 
num_letter(8, 'b'). 
num_letter(9, 'a').


% Last cell selector
last_num_cell(X, O) :- (X =< 5 -> O is X + 4 ; O is 5 + (9-X)).


% Ask for the positions of the pieces
pieces_to_move(Board, Player, (X1, Y1), (X2, Y2), (X3, Y3)) :-
    count_pieces(Board, Player, Count),
    write('You have '), write(Count), write(' pieces to move.'), nl,

    (Count >= 1 ->
    (write('Enter the first position (e.g. a2.) '),
    read(C1),
    sub_atom(C1, 0, 1, After11, Y1_LETTER),
    sub_atom(C1, 1, 1, After12, X1_CHAR),
    num_letter(Y4, Y1_LETTER),
    atom_codes(X1_CHAR, X1_CHAR_LIST),
    number_codes(X1_, X1_CHAR_LIST),
    offset(Y4, Offset1),
    X4 is Offset1 + X1_)),

    (Count >= 2 ->
    (write('Enter the second position (e.g. a2.) '),
    read(C2),
    sub_atom(C2, 0, 1, After21, Y2_LETTER),
    sub_atom(C2, 1, 1, After22, X2_CHAR),
    num_letter(Y5, Y2_LETTER),
    atom_codes(X2_CHAR, X2_CHAR_LIST),
    number_codes(X2_, X2_CHAR_LIST),
    offset(Y5, Offset2),
    X5 is Offset2 + X2_);true),

    (Count >= 3 -> 
    (write('Enter the third position (e.g. a2.) '),
    read(C3),
    sub_atom(C3, 0, 1, After31, Y3_LETTER),
    sub_atom(C3, 1, 1, After32, X3_CHAR),
    num_letter(Y6, Y3_LETTER),
    atom_codes(X3_CHAR, X3_CHAR_LIST),
    number_codes(X3_, X3_CHAR_LIST),
    offset(Y6, Offset3),
    X6 is Offset3 + X3_);true),

    ((Count == 1, X5 = X4, Y5 = Y4, X6 = X4, Y6 = Y4);
    (Count == 2, X6 = X5, Y6 = Y5);true),


    (check_positions(Board, Player, (X4, Y4), (X5, Y5), (X6, Y6)) ->
     (X1 = X4, X2 = X5, X3 = X6, Y1 = Y4, Y2 = Y5, Y3 = Y6); 
    pieces_to_move(Board, Player, (X1, Y1), (X2, Y2), (X3, Y3))).
  


move_dir(Direction, ValidMoves) :-
    write('Valid Moves: '), write(ValidMoves), nl,

    write('   North-West(1)    North-East(2)'), nl,
    write('West(3)                       East(4)'), nl,
    write('   South-West(5)    South-East(6)'), nl,

    write('Enter the direction in which you want to move (Choose from 1 to 6, according to the valid moves) '),
    read(Direction).


% Move a piece in the board
move_piece(Board, (FromRow, FromCol), (ToRow, ToCol), NewBoard) :-

    % First, retrieve the piece at the source position
    nth1(FromRow, Board, Row),
    nth1(FromCol, Row, Piece),

    % Then, replace the source position with an empty space
    replace(Row, FromCol, 0, NewRow),
    replace(Board, FromRow, NewRow, BoardWithoutPiece),

    % Finally, place the piece at the destination position
    nth1(ToRow, BoardWithoutPiece, ToRowData),
    replace(ToRowData, ToCol, Piece, NewToRowData),
    replace(BoardWithoutPiece, ToRow, NewToRowData, NewBoard).


% Replace an element in a list
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :- I > 1, I1 is I-1, replace(T, I1, X, R).


% Define the neighbors of a given position on the hexagonal board.
neighbors((X, Y), [NW, NE, E, SE, SW, W]) :-
    directions(Dirs),
    % Compute the coordinates of the neighbors in each direction.
    maplist(neighbor(X, Y), Dirs, [NW, NE, E, SE, SW, W]).


% Define a helper predicate to compute the coordinates of a neighbor in a given direction.
neighbor(X, Y, Dir, (NEW_X, NEW_Y)) :-
    ((Dir == 1; Dir == nw), NEW_X is X-1, NEW_Y is Y-1;
    (Dir == 2; Dir == ne), NEW_X is X, NEW_Y is Y-1;
    (Dir == 3; Dir == w), NEW_X is X-1, NEW_Y is Y;
    (Dir == 4; Dir == e), NEW_X is X+1, NEW_Y is Y;
    (Dir == 5; Dir == sw), NEW_X is X, NEW_Y is Y+1;
    (Dir == 6; Dir == se), NEW_X is X+1, NEW_Y is Y+1).


% Return a list of posible moves for a piece.
valid_moves(Board, [P1, P2, P3], Moves) :-
    % Compute the set of valid moves for each piece.
    directions(Dirs1),
    include(within_bounds(Board, P1), Dirs1, Moves1),  
     
    directions(Dirs2),
    include(within_bounds(Board, P2), Dirs2, Moves2),

    directions(Dirs3),
    include(within_bounds(Board, P3), Dirs3, Moves3),

    directions(Dirs4),
    include(not_occupied(Board, P1, [P2, P3]), Dirs4, Moves4),

    directions(Dirs5),
    include(not_occupied(Board, P2, [P1, P3]), Dirs5, Moves5),

    directions(Dirs6),
    include(not_occupied(Board, P3, [P1, P2]), Dirs6, Moves6),

    % Compute the intersection of the sets of valid moves for the three positions.
    intersection(Moves1, Moves2, Moves12),
    intersection(Moves12, Moves3, Moves123),
    intersection(Moves4, Moves5, Moves45),
    intersection(Moves45, Moves6, Moves456),
    intersection(Moves123, Moves456, Moves).


% Check if position is within the board
within_bounds(Board, (X, Y), Dir) :-
    neighbor(X, Y, Dir, (NEW_X, NEW_Y)),

    length(Board, N1),
    nth1(NEW_Y, Board, Row),
    length(Row, N2),
    NEW_Y > 0, NEW_Y =< N1,
    NEW_X > 0, NEW_X =< N2,
    nth1(NEW_Y, Board, Row),
    nth1(NEW_X, Row, Cell),
    \+(Cell =:= -1).


% Check if position is not occupied either by a own piece that is not in the list of pieces to move
not_occupied(Board, (X, Y), [(X1, Y1), (X2, Y2)], Dir) :-
    player_piece(Board, (X,Y), PlayerPieceNumber),
    neighbor(X, Y, Dir, (NEW_X, NEW_Y)),

    ((NEW_X == X1, NEW_Y == Y1);
    (NEW_X == X2, NEW_Y == Y2);

    (nth1(NEW_Y, Board, Row),
    nth1(NEW_X, Row, Cell),
    \+(Cell =:= PlayerPieceNumber))).


% Player piece number
player_piece(Board, (X, Y), PlayerPieceNumber) :-
    nth1(Y, Board, Row),
    nth1(X, Row, PlayerPieceNumber).


% intersection(A, B, C) return the intersection of sets A and B.
intersection([], _, []).
intersection([H|T], B, C) :-
    member(H, B),
    intersection(T, B, C2),
    C = [H|C2].
intersection([H|T], B, C) :-
    \+ member(H, B),
    intersection(T, B, C).


% Check if piece in winning row
check_player_in_win_row(Player, Board) :-
    % gest last row of Board
    (Player == 1 -> last(Board, Row); nth1(1, Board, Row)),
    % check if there are any player pieces in the row
    member(Player, Row).
    

% Get player pieces coordinates

get_player_pieces(Board, Player, Pieces) :-
    findall((X, Y), (nth1(Y, Board, Row), nth1(X, Row, Player)), Pieces).

count_pieces(Board, Player, Count) :-
    findall(1, (member(Row, Board), member(Player, Row)), List),
    length(List, Count).

check_positions(Board, Piece, (Col1, Row1), (Col2, Row2), (Col3, Row3)) :-
    nth1(Row1, Board, Row),
    nth1(Col1, Row, P1),
    P1 == Piece,
    nth1(Row2, Board, Row_),
    nth1(Col2, Row_, P2),
    P2 == Piece,
    nth1(Row3, Board, Row__),
    nth1(Col3, Row__, P3),
    P3 == Piece.