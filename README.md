# PFL_TP2_T2_06
Prolog Programming (Second project work)

 Game: Taacoca
    Group: 06
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

     - Game state view: description of the implementation of the game state view predicate. It
may include information about the created menu system, as well as user interaction,
including forms of input validation. The display predicate should be called
display_game(+GameState), receiving the current state of the game (which includes the
player who will make the next move). Appealing and intuitive visualizations will be
valued. Game state representations and the implementation of flexible visualization
predicates will also be valued, for example, working for any size of the board, using an
initial_state(+Size, -GameState) predicate that receives the size of the board as an
argument and returns the initial state about the game.
    
     - Moves Execution: Validation and execution of a move, obtaining the new state of the
game. The predicate should be called move(+GameState, +Move, -NewGameState).
    
     - List of Valid Moves: Obtaining a list of possible moves. The predicate should be called
valid_moves(+GameState, +Player, -ListOfMoves).
    
     - End of Game: Verification of the end of the game, with identification of the winner. The
predicate must be called game_over(+GameState, -Winner).

     - Board Evaluation: Form(s) of evaluating the state of the game. The predicate must be
called value(+GameState, +Player, -Value).
    
     - Computer move: Choice of the move to be performed by the computer, depending on
the difficulty level. The predicate should be called choose_move(+GameState, +Player,
+Level, -Move). Level 1 should return a random valid move. Level 2 should return the
best move at the moment (greedy algorithm), taking into account the evaluation of the
game state.

 - Conclusions: Conclusions of the work, including limitations of the work developed (known issues), as
well as possible improvements identified (roadmap) (up to 250 words);

 - Bibliography: Listing of books, articles, Web pages and other resources used during the development
https://www.iggamecenter.com/en/rules/taacoca
https://glukkazan.github.io/breakthrough/taacoca.htm
