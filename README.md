# Battle of Letters Game
This is a simple board game that can be played in the terminal. This is the first project assignment of Principles of Programming Languages course to become familiar with functional programming languages, such as Haskell. The Battle of the Letters is a player versus player game where the first three letters of the alphabet –A, B and C– are team up against the last letter of the alphabet –Z–. The first letters want to trap the last letter with leaving no room that it can move to and the last letter wants to pass to the otherside of the first letters. The first team which can reach to its goal wins the game, but both of the teams mustn’t exceed the maximum number of moves.

# How to Play?
- In order to play this game, firstly you should install Haskell on your own machine. Type `ghci` in order to start running the Haskell’s compiler. Then, load the source code with typing the command `:load BattleOfLetters.hs` and then, type `main` to start the game.
- There are several rules about moving the letters. The common rule for the both of the first letters and the last letter is, they can only move to empty cells which are indicated with – if and only if it is immediately next to or diagonally next to that letter which wanted to move.
- Additionally for the first letters, they can not move to backwards horizontally. In order to move one of the first letters, the input must include the letter which’ll be moved and which index it’ll be moved to.
- In order to move the last letter, the input must only include which index it’ll be moved to –since the only possible letter which’ll be moved is Z–.
- An invalid movement results in the turn passing to the next player and that movement is not counted. 
