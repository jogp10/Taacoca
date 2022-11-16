% Initiate the board

/* e.g.
   0 - empty
   1 - player 1
   2 - player 2 
*/
initBoard(Board) :-
    Board = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ].

% Display the board

displayBoard(Board) :-
    write('  0 1 2 3 4 5 6 7 8 9'), nl, nl,
    displayBoard(Board, 0).

displayBoard([], _).
displayBoard([H|T], N) :-
    write(N), write('  '),
    displayRow(H),
    NewN is N + 1,
    displayBoard(T, NewN).