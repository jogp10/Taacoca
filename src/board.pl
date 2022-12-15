:- use_module(library(lists)).

% Define the directions of the board
directions([nw, ne, e, se, sw, w]).

% Initiate the board
/* e.g.
  -2 - out of bounds
   0 - empty
   1 - player 1
   2 - player 2 
*/

initial_state(Board) :- Board = 
       [[1 , 1, 1, 1, 1,-3,-3,-3,-3],
        [0 , 1, 1, 1, 1, 0,-3,-3,-3],
        [0 , 0, 1, 1, 1, 0, 0,-3,-3],
        [0 , 0, 0, 0, 0, 0, 0, 0,-3],
        [0 , 0, 0, 0, 0, 0, 0, 0, 0],
        [-2, 0, 0, 0, 0, 0, 0, 0, 0],
        [-2,-2, 0, 0, 2, 2, 2, 0, 0],
        [-2,-2,-2, 0, 2, 2, 2, 2, 0],
        [-2,-2,-2,-2, 2, 2, 2, 2, 2]].
    

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
    display_game(Board, 0).

display_game([], _):- !.
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
    ((H == -3 -> write(''));
    (H == -2 -> write(' '));
    (H == -1 -> write('X '));
    (H == 0 -> write('- '));
    (H == 1 -> write('X '));
    (H == 2 -> write('O '))),
    display_row(T, C).
 

% Add spacing to the board

spacing(0) :- write('     ').
spacing(1) :- write('    ').
spacing(2) :- write('   ').
spacing(3) :- write('  ').
spacing(X) :- (X>3 -> write(' ')).


% Convert Num to Letter and Letter to Num
/*
    num_letter(0, X) : X -> 'I'
    num_letter(X, 'B'): X -> 7
*/
num_letter(0, 'I').
num_letter(1, 'H').
num_letter(2, 'G'). 
num_letter(3, 'F'). 
num_letter(4, 'E'). 
num_letter(5, 'D'). 
num_letter(6, 'C'). 
num_letter(7, 'B'). 
num_letter(8, 'A').


% Last cell selector

last_num_cell(X, O) :- (X < 5 -> O is X + 5 ; O is 5 + (8-X)).


% Ask for the positions of the pieces
pieces_to_move((X1, Y1), (X2, Y2), (X3, Y3)) :-
    write('Enter the first position (e.g [1,2].) '),
    read([X1, Y1]),

    write('Enter the second position (e.g [1,2].) '),
    read([X2, Y2]),

    write('Enter the third position (e.g [1,2].) '),
    read([X3, Y3]).


move_dir(Direction) :-
    nl,

    write('   North-West(1)    North-East(2)'), nl,
    write('West(3)                       East(4)'), nl,
    write('   South-West(5)    South-East(6)'), nl,

    write('Enter the direction in which you want to move (Choose from 1 to 8): '),
    read(Direction).


% offset for the board
offset(6, 1).
offset(7, 2).
offset(8, 3).
offset(9, 4).


% Move a piece in the board
move_piece(Board, (FromRow, FromCol), (ToRow, ToCol), NewBoard) :-

    % Convert the position to the correct format
    offset(FromRow, Offset),
    FromCol1 is FromCol + Offset,
    ToCol1 is ToCol + Offset,

    % First, retrieve the piece at the source position
    nth1(FromRow, Board, Row),
    write(Row),nl,
    nth1(FromCol1, Row, Piece),

    % Then, replace the source position with an empty space
    replace(Row, FromCol1, 0, NewRow),
    replace(Board, FromRow, NewRow, BoardWithoutPiece),

    % Finally, place the piece at the destination position
    nth1(ToRow, BoardWithoutPiece, ToRowData),
    replace(ToRowData, ToCol1, Piece, NewToRowData),
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
neighbor((X,Y), (NEW_X, NEW_Y), Dir) :-
    ((Dir == 1; Dir == nw), NEW_X is X-1, NEW_Y is Y-1;
    (Dir == 2; Dir == ne), NEW_X is X, NEW_Y is Y-1;
    (Dir == 3; Dir == w), NEW_X is X-1, NEW_Y is Y;
    (Dir == 4; Dir == e), NEW_X is X+1, NEW_Y is Y;
    (Dir == 5; Dir == sw), NEW_X is X, NEW_Y is Y+1;
    (Dir == 6; Dir == se), NEW_X is X+1, NEW_Y is Y+1).


% Return a list of posible moves for a piece.
valid_moves(Board, [P1, P2, P3], Moves) :-

    neighbors(P1, Neighbors),
    include(within_bounds(Board), Neighbors, Moves1),

    neighbors(P2, Neighbors),
    include(within_bounds(Board), Neighbors, Moves2),

    neighbors(P3, Neighbors),
    include(within_bounds(Board), Neighbors, Moves3),

    % Compute the intersection of the sets of valid moves for the three positions.
    intersection(Moves1, Moves2, Moves12),
    intersection(Moves12, Moves3, Moves).

% Check if position is within the board
within_bounds(Board, (X, Y)) :-
    length(Board, N),
    X > 0, X =< N,
    Y > 0, Y =< N,
    nth1(Y, Board, Row),
    nth1(X, Row, Cell),
    Cell \= -2,
    Cell \= -3.