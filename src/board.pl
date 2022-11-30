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