% Initiate the board

/* e.g.
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
%  [[0 , 0,0 ,0 ,0 ,-2,-2 ,-2,-2],[0 , 0,0 ,0 ,0 ,0 ,-2 ,-2,-2],[0 , 0,0 ,0 ,0 ,0 , 0 ,-2,-2],[0 , 0,0 ,0 ,0 ,0 , 0 ,0 ,-2],[0 , 0,0 ,0 ,0 ,0 , 0 ,0 , 0],[-2, 0,0 ,0 ,0 ,0 , 0 ,0 , 0],[-2,-2,0 ,0 ,0 ,0 , 0 ,0 , 0],[-2,-2,-2,0 ,0 ,0 , 0 ,0 , 0],[-2,-2,-2,-2,0 ,0 , 0 ,0 , 0]].

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

displayRow([]) :-
    nl.
displayRow([H|T]) :-
    ((H == -2 -> write(' '));
    (H == -1 -> write('X '));
    (H == 0 -> write('- '));
    (H == 1 -> write('1 '));
    (H == 2 -> write('2 '))),
    displayRow(T).
    
spacing(0) :- write('    ').
spacing(1) :- write('   ').
spacing(2) :- write('  ').
spacing(3) :- write(' ').
spacing(X) :- (X>3 -> write('')).
