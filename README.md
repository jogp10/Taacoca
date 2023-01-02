# Game: Taacoca

    Group: 06
    Class: 02
    Group members:
        - 202008133 - João de Oliveira Gigante Pinheiro
        - 202005103 - Ricardo Almeida Cavalheiro
    
    Contribution:
        - João de Oliveira Gigante Pinheiro: 50%
        - Ricardo Almeida Cavalheiro: 50%

- Installation and Execution:
  
    To run the game, you must first install SICStus Prolog 4.7.1. After that, you must load the file "main.pl" in SICStus Prolog and run the predicate "play/0". The game will start and you will be able to play it.


- Game Description:

    Taacoca is played on a hexagonal board with 19 hexagons. Each player has 12 stones. The bottom row is the Home row for the White player, the top row is the Home row for the Green player. The initial position of the stones is shown in the figure below. The game is played by two players, White and Green. The players take turns to move their stones. The goal of the game is to move one of your stones to the Home row of your opponent. The game ends when one player has no more stones on the board or there aren't any valid moves. The player to reach first the Home row of his opponent wins the game. No draw is possible in Taacoca.

    Starting with White, players take turns moving any three of their stones one cell (if a player has less than three stones he must move all the remaining stones). The chosen stones do not need to be connected to each other but they must move in the same direction.
    A player cannot move his stones if one of the target cells is occupied with another of his stones. If any of the target cells are occupied with the opponent's stones then the opponent's stones are captured and removed from the board.
    https://www.iggamecenter.com/en/rules/taacoca


 - Game Logic: Describe (don't just copy the source code) the design and implementation of game logic
in Prolog. The game start predicate must be play/0. This section should have information on the
following topics (up to 2400 words in total):

     - Internal representation of the state of the game: indication of how they represent the
state of the game, including board (typically using list of lists with different atoms for the
pieces), current player, and eventually captured pieces and/or still to be played, or other
information that may be needed (depending on the game). It should include examples of
the Prolog representation of initial, intermediate and final game states, and an indication
of the meaning of each atom (ie., how the different pieces represent).

    The game has two players and their pieces are represented internally by 1 and 2 and displayed to the user using 'X' and 'O'. A blank cell is represented by a 0 and outside of the board has a 2.
    The board is represented by a list of lists, where each inner list represents a row of the game board and each element of the inner list represents a cell on the    board. A message is displayed when each player has their chance to move, requesting the positions of the pieces and the direction they want them to move. When pieces are captured they are removed from the board.
    ```
    Initial state of the game:
    
     [[1 , 1, 1, 1, 1,-1,-1,-1,-1],
      [0 , 1, 1, 1, 1, 0,-1,-1,-1],
      [0 , 0, 1, 1, 1, 0, 0,-1,-1],
      [0 , 0, 0, 0, 0, 0, 0, 0,-1],
      [0 , 0, 0, 0, 0, 0, 0, 0, 0],
      [-1, 0, 0, 0, 0, 0, 0, 0, 0],
      [-1,-1, 0, 0, 2, 2, 2, 0, 0],
      [-1,-1,-1, 0, 2, 2, 2, 2, 0],
      [-1,-1,-1,-1, 2, 2, 2, 2, 2]]
    ```
     ```
    Intermediate state of the game:
    Player 1 has 9 pieces remaining..
    Player 2 has 8 pieces remaining..
    
     [[0 , 0, 0, 0, 0,-1,-1,-1,-1],
      [0 , 0, 1, 1, 0, 0,-1,-1,-1],
      [0 , 0, 1, 1, 1, 0, 0,-1,-1],
      [0 , 0, 0, 1, 0, 0, 0, 0,-1],
      [0 , 0, 2, 1, 1, 1, 0, 0, 0],
      [-1, 0, 2, 2, 0, 0, 0, 0, 0],
      [-1,-1, 2, 2, 0, 2, 0, 0, 0],
      [-1,-1,-1, 0, 0, 0, 0, 2, 0],
      [-1,-1,-1,-1, 0, 0, 0, 2, 0]]
    ```
     ```
    Final state of the game:
    Player 1 has reached the end of the Board.
    Player 1 WON.
    
      [[0 , 0, 0, 0, 0,-1,-1,-1,-1],
       [0 , 0, 1, 0, 1, 0,-1,-1,-1],
       [0 , 0, 0, 0, 0, 0, 0,-1,-1],
       [0 , 0, 0, 1, 0, 0, 0, 0,-1],
       [0 , 0, 0, 1, 1, 1, 0, 0, 0],
       [-1, 0, 0, 2, 0, 0, 0, 0, 0],
       [-1,-1, 0, 0, 0, 0, 2, 0, 0],
       [-1,-1,-1, 0, 1, 0, 2, 2, 0],
       [-1,-1,-1,-1, 1, 0, 0, 0, 0]]
    ```

     - Game state view: description of the implementation of the game state view predicate. It
may include information about the created menu system, as well as user interaction,
including forms of input validation. The display predicate should be called
display_game(+GameState), receiving the current state of the game (which includes the
player who will make the next move). Appealing and intuitive visualizations will be
valued. Game state representations and the implementation of flexible visualization
predicates will also be valued, for example, working for any size of the board, using an
initial_state(+Size, -GameState) predicate that receives the size of the board as an
argument and returns the initial state about the game.
    
     The game state view predicate, called display_game/1, is responsible for displaying the current state of the game to the user. It takes the current game state as an argument and displays the board and the pieces on it, as well as the current player and any captured pieces.
     The predicate checks if the game has ended by calling the game_over/2 predicate, which returns true if the game has ended and false otherwise. If the game has ended, the predicate displays a message indicating which player has won. If the game has not ended, the predicate displays the current player and the number of pieces each player has remaining. It then displays the board and the pieces on it.
     There is a Menu System at the beginning of the game where the user must choose to play one of the following game modes: Player vs Player; Player vs Computer;
     Besides that, there is a game mode where the user can watch the game being played by two computers (AI vs AI). 
     The user can also choose the difficulty of the computer, which can be easy or hard.
     We implemented input validation so that the user can only choose positions of the board where he has pieces and the game also checks if the direction he provided is valid.

    
     - Moves Execution: Validation and execution of a move, obtaining the new state of the
game. The predicate should be called move(+GameState, +Move, -NewGameState).

     The move predicate, called move/3, is responsible for executing a move in the game. It takes the current game state, the move to be executed, and returns the new game state after the move has been made.
     The predicate first checks that the move is valid by calling the valid_moves/3 predicate, which returns a list of all the valid moves that can be made from the current game state. If the move is not valid, the predicate returns an error message. If the move is valid, the predicate will execute the move by removing the stones from the current position and placing them in the new position. If the move captures any opponent's stones, the predicate will remove them from the board.

     - List of Valid Moves: Obtaining a list of possible moves. The predicate should be called
valid_moves(+GameState, +Player, -ListOfMoves).

     The valid_moves predicate, called valid_moves/3, is responsible for generating a list of all the valid moves that can be made from the current game state. It takes the current game state and the player making the move as arguments and returns a list of all the valid moves. The predicate calls the combination_size3/2, which returns all the possible combinations of 3 stones from the current game state, and then calls the findall/3, which checks if the combination with a possible direction is a valid move and returns it if it is.
    
     - End of Game: Verification of the end of the game, with identification of the winner. The
predicate must be called game_over(+GameState, -Winner).

     The game_over predicate, called game_over/2, is responsible for checking if the game is over. It takes the current game state as an argument and returns the winner of the game, if there is one. If the game is not over, it returns the atom 'none'.
     The predicate first checks if the current player has any valid moves. If not, it checks if the player has any stones on the Win row or the opponent has no stones left. If neither, it will check the same for the player number 2. One condition is match and the game is over, the predicate returns the winner.

     - Board Evaluation: Form(s) of evaluating the state of the game. The predicate must be
called value(+GameState, +Player, -Value).

     The value predicate, called value/3, is responsible for evaluating the current game state. It takes the current game state and the player making the move as arguments and returns a value representing the current state of the game. 
     The value is calculated by counting the number of stones of each player and subtracting the number of stones for the opponent from the number of stones for the player. This value is then multiplied by 3. The distance to the winning row is also taken into account, with each stone being worth 1 point for each row closer to the winning row.
     A win is worth 100 points, and a loss is worth -100 points.
    
     - Computer move: Choice of the move to be performed by the computer, depending on
the difficulty level. The predicate should be called choose_move(+GameState, +Player,
+Level, -Move). Level 1 should return a random valid move. Level 2 should return the
best move at the moment (greedy algorithm), taking into account the evaluation of the
game state.

     The computer_easy_move predicate, called computer_easy_move/3, is responsible for choosing the best move to be made by the easy difficulty computer. It takes the current game state and the player making the move and returns the best move to be made.
     The predicate calls the random_member/2 predicate, which returns a random valid move.
     If the difficulty level is hard, the computer_hard_move/3 is called. This predicate returns the best move to be made by the hard difficulty computer. It takes the current game state and the player making the move and returns the best move to be made. The predicates evaluates all the valid moves and returns the move with the highest value.
    

 - Conclusions: Conclusions of the work, including limitations of the work developed (known issues), as
well as possible improvements identified (roadmap) (up to 250 words);

     In this project, we developed a board game in Prolog called Taacoca. The game implemented the unique rules and gameplay mechanics of Taacoca, including movement of the pieces, capture of opponent's pieces and the end of the game.
     Overall, the game was functional and allowed users to play Taacoca against the computer or another player. However, there were some limitations to the project. The game's user interface was basic, with only text-based representations of the game board and pieces.
     In future work, it would be interesting to enhance the user interface with more visually appealing graphics. Additionally, optimizing the performance of the game for larger board sizes and more complex variations of the game could be a worthwhile goal.
     Overall, this project was a valuable learning experience and provided an opportunity to apply our knowledge of Prolog to the development of a unique and enjoyable board game.
     Some of the issues we faced were:
     -Performance: Board games can be computationally intensive, and Prolog can struggle with the performance demands of larger board sizes or more complex variations of the game. This can lead to slow or unresponsive gameplay.
     -Complexity: Board games can have a large number of rules and interactions to implement, which can make the Prolog code challenging to write and maintain.
     -Debugging: Debugging Prolog code can be difficult due to the logic-based nature of the language. This can make it challenging to identify and fix errors in the game logic.
     -User interface: Creating a visually appealing and intuitive user interface can be difficult in Prolog, as the language is not well-suited to graphical output.
     -AI: Implementing a strong artificial intelligence for the computer player can be a challenging task, especially for more complex board games.
     Overall, developing board games in Prolog can be a rewarding but challenging task that requires careful planning and attention to performance and usability.


 - Bibliography
https://www.iggamecenter.com/en/rules/taacoca
https://glukkazan.github.io/breakthrough/taacoca.htm
