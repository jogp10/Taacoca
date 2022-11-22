% Initiate the board
/* e.g.
  -2 - out of bounds
   0 - empty
   1 - player 1
   2 - player 2 
*/

initBoard(Board) :-
    Board = 
       [[0 , 0, 0, 0, 0,-2,-2 ,-2,-2],
        [0 , 0, 0, 0, 0, 0,-2 ,-2,-2],
        [0 , 0, 0, 0, 0, 0, 0 ,-2,-2],
        [0 , 0, 0, 0, 0, 0, 0 ,0 ,-2],
        [0 , 0, 0, 0, 0, 0, 0 ,0 , 0],
        [-2, 0, 0, 0, 0, 0, 0 ,0 , 0],
        [-2,-2, 0, 0, 0, 0, 0 ,0 , 0],
        [-2,-2,-2, 0, 0, 0, 0 ,0 , 0],
        [-2,-2,-2,-2, 0, 0, 0 ,0 , 0]
].
    

% Display the board
/* e.g.
    -     1 2 3 4 5
    I     - - - - -
    H    - - - - - - 6
    G   - - - - - - - 7
    F  - - - - - - - - 8
    E - - - - - - - - - 9
    D  - - - - - - - - 8
    C   - - - - - - - 7
    B    - - - - - - 6
    A     - - - - -
          1 2 3 4 5

*/

displayBoard(Board) :-
    write('  0 1 2 3 4 5 6 7 8 9'), nl,
    displayBoard(Board, 0).

displayBoard([], _).
displayBoard([H|T], N) :-
    write(N), write(' '),
    spacing(N),
    displayRow(H),
    nl,
    NewN is N + 1,
    displayBoard(T, NewN).


% Display a row of the board
/* e.g.
    A     - - - - -
*/

displayRow([]) :-
    nl.
displayRow([H|T]) :-
    ((H == -2 -> write(' '));
    (H == -1 -> write('X '));
    (H == 0 -> write('- '));
    (H == 1 -> write('1 '));
    (H == 2 -> write('2 '))),
    displayRow(T).
 

% Add spacing to the board

spacing(0) :- write('    ').
spacing(1) :- write('   ').
spacing(2) :- write('  ').
spacing(3) :- write(' ').
spacing(X) :- (X>3 -> write('')).
