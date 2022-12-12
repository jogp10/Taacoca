:- use_module(library(lists)).

% Initiate the board
/* e.g.
  -2 - out of bounds
   0 - empty
   1 - player 1
   2 - player 2 
*/

initBoard(Board) :- Board = 
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

displayBoard(Board) :-
    write('       1 2 3 4 5'), nl,
    displayBoard(Board, 0).

displayBoard([], _):- !.
displayBoard([H|T], N) :-
    numToABC(N, RowLetter),
    write(RowLetter), write(' '),
    spacing(N),
    lastCellNum(N, LastCellNum),
    displayRow(H, LastCellNum),
    nl,
    NewN is N + 1,
    displayBoard(T, NewN).


% Display a row of the board
/* e.g.
    A     - - - - -
*/

displayRow([], C) :-  write(' '), write(C),  nl.
displayRow([H|T], C) :-
    ((H == -3 -> write(''));
    (H == -2 -> write(' '));
    (H == -1 -> write('X '));
    (H == 0 -> write('- '));
    (H == 1 -> write('X '));
    (H == 2 -> write('O '))),
    displayRow(T, C).
 

% Add spacing to the board

spacing(0) :- write('     ').
spacing(1) :- write('    ').
spacing(2) :- write('   ').
spacing(3) :- write('  ').
spacing(X) :- (X>3 -> write(' ')).


% Convert Num to Letter and Letter to Num
/*
    numToABC(0, X) : X -> 'I'
    numToABC(X, 'B'): X -> 7
*/

numToABC(0, 'I').
numToABC(1, 'H').
numToABC(2, 'G'). 
numToABC(3, 'F'). 
numToABC(4, 'E'). 
numToABC(5, 'D'). 
numToABC(6, 'C'). 
numToABC(7, 'B'). 
numToABC(8, 'A').


% Last cell selector

lastCellNum(X, O) :- (X < 5 -> O is X + 5 ; O is 5 + (8-X)).

% Ask for the positions of the pieces

ask_for_positions(X1, Y1, X2, Y2, X3, Y3) :-
    write('Enter the first position (e.g [1,2].) '),
    read([X1, Y1]),

    write('Enter the second position (e.g [1,2].) '),
    read([X2, Y2]),

    write('Enter the third position (e.g [1,2].) '),
    read([X3, Y3]).

ask_for_direction(Direction) :-
    nl,

    write('   North-West(1)    North-East(2)'), nl,
    write('West(3)                       East(4)'), nl,
    write('   South-West(5)    South-East(6)'), nl,


    write('Enter the direction in which you want to move (Choose from 1 to 8): '),
    read(Direction).

% Move a piece in the board
move_piece(Board, FromRow, FromCol, ToRow, ToCol, NewBoard) :-

    % First, retrieve the piece at the source position
    nth1(FromRow, Board, Row),
    write(Row),nl,
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

%1-North, 2-North-West, 3-North-East, 4-West, 5-East, 6-South-West, 7-South-East, 8-South
get_new_position(Board,X1,Y1,Direction,A,B) :-
    ((Direction == 1 -> A is X1-1, B is Y1-1);
    (Direction == 2 -> A is X1, B is Y1-1);
    (Direction == 3 -> A is X1-1, B is Y1);
    (Direction == 4 -> A is X1+1, B is Y1);
    (Direction == 5 -> A is X1, B is Y1+1);
    (Direction == 6 -> A is X1+1, B is Y1+1);).




