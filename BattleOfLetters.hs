-- CSE 2260 - Spring 2025 - Project #1
-- Eren Emre Aycibin
-- Furkan Eren Gülçay
-- Yasin Emre Çetin

import Data.List
import Control.Exception

-- Some constants related to the game board.
minRowIndex :: Int
minRowIndex = 0
maxRowIndex :: Int
maxRowIndex = 2
minColumnIndex :: Int
minColumnIndex = 0
maxColumnIndex :: Int
maxColumnIndex = 4
numOfRows :: Int
numOfRows = 3
numOfColumns :: Int
numOfColumns = 5
minIndex :: Int
minIndex = 0
maxIndex :: Int
maxIndex = 14

-- Prints the content of the game board.
printBoard :: [[String]] -> IO ()
printBoard board 
    | null board = return ()
    | not $ null board = do
        putStrLn $ intercalate " " $ head board
        printBoard $ tail board

-- Gets the row index from position information.
getRow :: Int -> Int
getRow position = position `div` numOfColumns

-- Gets the column index from position information.
getColumn :: Int -> Int
getColumn position = position `rem` numOfColumns

-- Finds the asked letter in the game board and returns its position.
findInBoard :: [[String]] -> String -> Int -> Int
findInBoard board letter currentIndex 
    | board !! rowIndex !! columnIndex /= letter = findInBoard board letter $ currentIndex + 1
    | otherwise = currentIndex
    where
        rowIndex = getRow currentIndex
        columnIndex = getColumn currentIndex

-- Updates the single row of the board.
updateBoardRow :: Int -> String -> [String] -> [String]
updateBoardRow index letter boardRow = take index boardRow ++ [letter] ++ drop (index + 1) boardRow

-- Updates the board content.
updateBoard :: [[String]] -> String -> Int -> [[String]]
updateBoard board source target = do
    let updatedSource = take sourceRow board ++ [updateBoardRow sourceColumn "-" (board !! sourceRow)] ++ drop (sourceRow + 1) board
    take targetRow updatedSource ++ [updateBoardRow targetColumn source (updatedSource !! targetRow)] ++ drop (targetRow + 1) updatedSource
    where
        sourceRow = getRow $ findInBoard board source minIndex
        sourceColumn = getColumn $ findInBoard board source minIndex
        targetRow = getRow target 
        targetColumn = getColumn target 

-- Gets the maximum number of total moves from user.
getMaximumMoves :: IO Int
getMaximumMoves = do
    x <- getLine
    result <- try (evaluate (read x :: Int)) :: IO (Either SomeException Int)
    case result of
        Left exception -> do
            putStr "Invalid number of moves. Try again: "
            getMaximumMoves
        Right moves -> do
            if moves > 0 then 
                return moves
            else do
                putStr "Invalid number of moves. Try again: "
                getMaximumMoves

-- Gets the information about will game continue from user.
getWillContinue :: IO Bool
getWillContinue = do
    x <- getLine
    if x == "yes" then 
        return True
    else if x == "no" then 
        return False
    else do
        putStr "Invalid choice. Try again: "
        getWillContinue
        
-- Gets the information about who starts first from user.
getFirstStarter :: IO String
getFirstStarter = do
    firstStarter <- getLine
    if firstStarter == "firsts" || firstStarter == "last" then 
        return firstStarter
    else do
        putStr "Invalid choice. Try again: "
        getFirstStarter

-- Checks for the input movement is valid or not.
isValidMove :: [[String]] -> String -> String -> Int -> Bool
isValidMove board currentTurn letter targetPos 
    | not (minIndex <= targetPos && targetPos <= maxIndex) = False
    | not isValidTarget = False
    | (board !! targetRow !! targetColumn) /= "-" = False
    | otherwise = True
    where
        currentPos = findInBoard board letter minIndex
        currentRow = getRow currentPos 
        currentColumn = getColumn currentPos 
        targetRow = getRow targetPos 
        targetColumn = getColumn targetPos 
        isValidTarget
            | currentTurn == "firsts" = currentRow - 1 <= targetRow && targetRow <= currentRow + 1 && currentColumn <= targetColumn && targetColumn <= currentColumn + 1
            | otherwise = currentRow - 1 <= targetRow && targetRow <= currentRow + 1 && currentColumn - 1 <= targetColumn && targetColumn <= currentColumn + 1

-- Checks that if Z passed the otherside of the firsts.
isLastWin :: [[String]] -> Bool
isLastWin board = posZ < posA && posZ < posB && posZ < posC
    where
        posA = getColumn $ findInBoard board "A" minIndex
        posB = getColumn $ findInBoard board "B" minIndex
        posC = getColumn $ findInBoard board "C" minIndex
        posZ = getColumn $ findInBoard board "Z" minIndex
    
-- Checks that if Z can move or not.
isFirstsWin :: [[String]] -> Bool
isFirstsWin board 
    | rowZ == minRowIndex = 
        not $ board !! rowZ !! (columnZ - 1) == "-" || 
        board !! rowZ !! (columnZ + 1) == "-" || 
        board !! (rowZ + 1) !! (columnZ - 1) == "-" ||
        board !! (rowZ + 1) !! columnZ == "-" || 
        board !! (rowZ + 1) !! (columnZ + 1) == "-"
    | rowZ == maxRowIndex = 
        not $ board !! rowZ !! (columnZ - 1) == "-" ||
        board !! rowZ !! (columnZ + 1) == "-" ||
        board !! (rowZ - 1) !! (columnZ - 1) == "-" ||
        board !! (rowZ - 1) !! columnZ == "-" ||
        board !! (rowZ - 1) !! (columnZ + 1) == "-"
    | columnZ == minColumnIndex = 
        not $ board !! (rowZ - 1) !! columnZ == "-" ||
        board !! (rowZ - 1) !! (columnZ + 1) == "-" ||
        board !! rowZ !! (columnZ + 1) == "-" ||
        board !! (rowZ + 1) !! columnZ == "-" ||
        board !! (rowZ + 1) !! (columnZ + 1) == "-"
    | columnZ == maxColumnIndex = 
        not $ board !! (rowZ - 1) !! (columnZ - 1) == "-" ||
        board !! (rowZ - 1) !! columnZ == "-" ||
        board !! rowZ !! (columnZ - 1) == "-" ||
        board !! (rowZ + 1) !! (columnZ - 1) == "-" ||
        board !! (rowZ + 1) !! columnZ == "-"
    | otherwise = 
        not $ board !! (rowZ - 1) !! (columnZ - 1) == "-" ||
        board !! (rowZ - 1) !! columnZ == "-" ||
        board !! (rowZ - 1) !! (columnZ + 1) == "-" ||
        board !! rowZ !! (columnZ - 1) == "-" ||
        board !! rowZ !! (columnZ + 1) == "-" ||
        board !! (rowZ + 1) !! (columnZ - 1) == "-" ||
        board !! (rowZ + 1) !! columnZ == "-" || 
        board !! (rowZ + 1) !! (columnZ + 1) == "-"
    where
        rowZ = getRow $ findInBoard board "Z" minIndex
        columnZ = getColumn $ findInBoard board "Z" minIndex

-- Game continues until someone wins or maximum move is reached.
gameLoop :: [[String]] -> String -> Int -> Int -> IO()
gameLoop board currentTurn maxMoves currentMoves 
    | isFirstsWin board = do
        putStrLn "-- Game is ended. A, B and C won! --"
        printBoard board
    | isLastWin board = do
        putStrLn "-- Game is ended. Z won! --"
        printBoard board
    | currentMoves >= maxMoves = do
        putStrLn "-- Game is ended. It is draw! --"
        printBoard board
    | currentTurn == "firsts" = do
        putStr $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Select one of the first letters and a cell to move it (e.g., A 6): "
        input <- getLine
        let inputAsList = words input
        if length inputAsList /= 2 then do
            putStrLn $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Invalid move." 
            gameLoop board "last" maxMoves currentMoves
        else do
            let letter = inputAsList !! 0
            let cellAsString = inputAsList !! 1
            if letter /= "A" && letter /= "B" && letter /= "C" then do
                putStrLn $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Invalid move." 
                gameLoop board "last" maxMoves currentMoves
            else do
                cell <- try (evaluate (read cellAsString :: Int)) :: IO (Either SomeException Int)
                case cell of
                    Left exception -> do
                        putStrLn $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Invalid move." 
                        gameLoop board "last" maxMoves currentMoves
                    Right target -> do
                        if isValidMove board currentTurn letter target then do
                            let updatedBoard = updateBoard board letter target
                            printBoard updatedBoard
                            gameLoop updatedBoard "last" maxMoves $ currentMoves + 1
                        else do
                            putStrLn $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Invalid move." 
                            gameLoop board "last" maxMoves currentMoves
    | otherwise = do
        putStr $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Select a cell to move Z (e.g., 8): "
        input <- getLine
        cell <- try (evaluate (read input :: Int)) :: IO (Either SomeException Int)
        case cell of
            Left exception -> do
                putStrLn $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Invalid move." 
                gameLoop board "firsts" maxMoves currentMoves
            Right target -> do
                if isValidMove board currentTurn "Z" target then do
                    let updatedBoard = updateBoard board "Z" target
                    printBoard updatedBoard
                    gameLoop updatedBoard "firsts" maxMoves $ currentMoves + 1
                else do
                    putStrLn $ "[" ++ show currentMoves ++ "/" ++ show maxMoves ++ "] Invalid move." 
                    gameLoop board "firsts" maxMoves currentMoves

-- Starts the game.
startGame :: Bool -> IO()
startGame willContinue 
    | not willContinue = putStrLn "Good Bye!"
    | willContinue = do
        let board = [["X", "A", "-", "-", "X"], 
                     ["B", "-", "-", "-", "Z"], 
                     ["X", "C", "-", "-", "X"]]
        putStrLn "\nWelcome!"
        printBoard board
        putStr "Enter the maximum number of total moves allowed: "
        moves <- getMaximumMoves 
        putStr "Who starts first? (firsts / last): "
        firstStarter <- getFirstStarter 
        gameLoop board firstStarter moves 0
        putStr "Another round? (yes / no): "
        anotherRound <- getWillContinue
        startGame anotherRound

-- Runs the code.
main :: IO ()
main = do
    startGame True
    