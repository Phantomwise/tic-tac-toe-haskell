-- Tic Tac Toe


-- Imports
import Data.Char (isSpace)
import System.Directory (doesFileExist)


-- Enable/disable debug messages
debug :: Bool
debug = True


-- Print debug messages
printDebug :: String -> IO ()
printDebug msg =
    if debug
        then putStrLn (ansi Magenta ++ "[DEBUG] " ++ ansi Reset ++ msg)
        else return ()


-- Define a new type for the content of the cells
data Cell = Empty | X | O
    deriving (Eq)
    -- deriving (Eq, Show)

instance Show Cell where
    show Empty = "·"
    show X = "X"
    show O = "O"


-- Define a new type for colors
data AnsiColor = Red | Green | Yellow | Blue | Magenta | Cyan | Reset
    deriving (Eq, Enum, Show)

-- Map to ANSI color codes
ansi :: AnsiColor -> String
ansi Red     = "\ESC[0;31m"
ansi Green   = "\ESC[0;32m"
ansi Yellow  = "\ESC[0;33m"
ansi Blue    = "\ESC[0;34m"
ansi Magenta = "\ESC[0;35m"
ansi Cyan    = "\ESC[0;36m"
ansi Reset   = "\ESC[0m"


-- Files
historyFile = "history.csv" :: FilePath


-- Cells IDs
-- cellsIDs     = [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- ┏━━━┳━━━┳━━━┓
-- ┃ 7 ┃ 8 ┃ 9 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 4 ┃ 5 ┃ 6 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 1 ┃ 2 ┃ 3 ┃
-- ┗━━━┻━━━┻━━━┛


-- Mapping cells IDs to rows, columns and diagonals
-- NB: Not actually used for anything
row1ids, row2ids, row3ids, column1ids, column2ids, column3ids, diagonal1ids, diagonal2ids :: [Int]
row1ids      = [7, 8, 9]
row2ids      = [4, 5, 6]
row3ids      = [1, 2, 3]
column1ids   = [7, 4, 1]
column2ids   = [8, 5, 2]
column3ids   = [9, 6, 3]
diagonal1ids = [7, 5, 3]
diagonal2ids = [9, 5, 1]


-- Function to get the name of Player X
getPlayerX :: IO String
getPlayerX = do
    putStr (ansi Yellow ++ "Enter the name of Player X: " ++ ansi Reset)
    playerX <- getLine
    printDebug ("getPlayerX: playerX = " ++ ansi Cyan ++ show playerX ++ ansi Reset)
    if (null playerX == True) || (all isSpace playerX == True)
        then do
            putStrLn (ansi Red ++ "The name of Player X can't be empty or whitespace." ++ ansi Reset)
            getPlayerX
        else do
            putStrLn (ansi Yellow ++ "The name of Player X is " ++ ansi Reset ++ playerX)
            return playerX


-- Function to get the name of Player O
getPlayerO :: IO String
getPlayerO = do
    putStr (ansi Yellow ++ "Enter the name of Player O: " ++ ansi Reset)
    playerO <- getLine
    printDebug ("getPlayerO: playerO = " ++ ansi Cyan ++ show playerO ++ ansi Reset)
    if (null playerO == True) || (all isSpace playerO == True)
        then do
            putStrLn (ansi Yellow ++ "The name of Player O can't be empty or whitespace." ++ ansi Reset)
            getPlayerO
        else do
            putStrLn (ansi Yellow ++ "The name of Player X is " ++ ansi Reset ++ playerO)
            return playerO


-- Function to get the name of Player B
getPlayerNameB :: IO String
getPlayerNameB = do
    putStr (ansi Yellow ++ "Enter the name of Player B:  ")
    playerB <- getLine
    printDebug ("getPlayerNameB: playerB = " ++ ansi Cyan ++ show playerB)
    return playerB


-- Function to print the current board
printBoard :: [Cell] -> IO ()
printBoard board = do

    -- Map cells to board index
    let cell1, cell2, cell3, cell4, cell5, cell6, cell7, cell8, cell9 :: Cell
        cell1 = board !! 0
        cell2 = board !! 1
        cell3 = board !! 2
        cell4 = board !! 3
        cell5 = board !! 4
        cell6 = board !! 5
        cell7 = board !! 6
        cell8 = board !! 7
        cell9 = board !! 8

    printDebug ("printBoard: Printing the current board:")
    putStrLn "┏━━━┳━━━┳━━━┓"
    putStrLn ("┃ " ++ show cell7 ++ " ┃ " ++ show cell8 ++ " ┃ " ++ show cell9 ++ " ┃")
    putStrLn "┣━━━╋━━━╋━━━┫"
    putStrLn ("┃ " ++ show cell4 ++ " ┃ " ++ show cell5 ++ " ┃ " ++ show cell6 ++ " ┃")
    putStrLn "┣━━━╋━━━╋━━━┫"
    putStrLn ("┃ " ++ show cell1 ++ " ┃ " ++ show cell2 ++ " ┃ " ++ show cell3 ++ " ┃")
    putStrLn "┗━━━┻━━━┻━━━┛"
    putStrLn ""


-- Function to validate user input and convert it to an Int, brute force version
validateUserInput :: Char -> Either String Int
validateUserInput k
    | k == '1' = Right (1 :: Int)
    | k == '2' = Right (2 :: Int)
    | k == '3' = Right (3 :: Int)
    | k == '4' = Right (4 :: Int)
    | k == '5' = Right (5 :: Int)
    | k == '6' = Right (6 :: Int)
    | k == '7' = Right (7 :: Int)
    | k == '8' = Right (8 :: Int)
    | k == '9' = Right (9 :: Int)
    | otherwise = Left "Invalid input. Please enter a number from 1 to 9."


-- Function to get user input and validate it
playerUserInput :: IO Int
playerUserInput = do
    putStrLn (ansi Yellow ++ "Press a number from 1 to 9 to select a cell to play (numpad order)" ++ ansi Reset)
    k <- getChar
    _ <- getLine -- Trash everything after the first character and be mad at having to deal with newlines and buffers >_<
    case validateUserInput k of
        Left err -> do
            printDebug ("playerUserInput: Key pressed: " ++ ansi Cyan ++ show k ++ ansi Reset)
            putStrLn (ansi Red ++ "ERROR: " ++ ansi Reset ++ "Invalid input, please try again.")
            printDebug ("playerUserInput: User input invalid: " ++ ansi Cyan ++ show (validateUserInput k) ++ ansi Reset)
            playerUserInput
        Right n -> do
            printDebug ("playerUserInput: Key pressed: " ++ ansi Cyan ++ show k ++ ansi Reset)
            printDebug ("playerUserInput: User input validated: " ++ "validateUserInput k = " ++ ansi Cyan ++ show (validateUserInput k) ++ ansi Reset)
            printDebug ("playerUserInput: User input validated: " ++ "n = " ++ ansi Cyan ++ show n ++ ansi Reset)
            putStrLn ""
            return n


-- Function to update the board
updateCell :: Int -> Cell -> [Cell] -> [Cell]
updateCell cellNb symbol xs = take (cellNb - 1) xs ++ [symbol] ++ drop cellNb xs


-- Player for each move
playerMove :: Int ->  Cell
playerMove m
    | odd m = X
    | otherwise = O


{-
-- Occupancy boolean
isOccupied :: Cell -> Bool
isOccupied X = True
isOccupied O = True
isOccupied Empty = False

-- Check occupancy
isOccupiedAt :: [Cell] -> Int -> Bool
isOccupiedAt board cellNb = isOccupied (board !! (cellNb - 1))
-}


-- Check occupancy
isOccupiedAt :: [Cell] -> Int -> Bool
isOccupiedAt board cellNb
    | (board !! (cellNb - 1)) == X = True
    | (board !! (cellNb - 1)) == O = True
    | otherwise = False


-- Check for win conditions
winCheck :: [Cell] -> Maybe Cell
winCheck [X,X,X,_,_,_,_,_,_] = Just X
winCheck [_,_,_,X,X,X,_,_,_] = Just X
winCheck [_,_,_,_,_,_,X,X,X] = Just X
winCheck [X,_,_,X,_,_,X,_,_] = Just X
winCheck [_,X,_,_,X,_,_,X,_] = Just X
winCheck [_,_,X,_,_,X,_,_,X] = Just X
winCheck [X,_,_,_,X,_,_,_,X] = Just X
winCheck [_,_,X,_,X,_,X,_,_] = Just X
winCheck [O,O,O,_,_,_,_,_,_] = Just O
winCheck [_,_,_,O,O,O,_,_,_] = Just O
winCheck [_,_,_,_,_,_,O,O,O] = Just O
winCheck [O,_,_,O,_,_,O,_,_] = Just O
winCheck [_,O,_,_,O,_,_,O,_] = Just O
winCheck [_,_,O,_,_,O,_,_,O] = Just O
winCheck [O,_,_,_,O,_,_,_,O] = Just O
winCheck [_,_,O,_,O,_,O,_,_] = Just O
winCheck _ = Nothing


-- Check remaining free cells
fullBoard :: [Cell] -> Bool
fullBoard board = if elem Empty board then False else True


gameLoop :: [Cell] -> Int -> (String,String) -> IO ()
gameLoop board move (playerX, playerO) = do
    printBoard board
    printDebug ("gameLoop: board = " ++ ansi Cyan ++ show board ++ ansi Reset)
    printDebug ("gameLoop: move = " ++ ansi Cyan ++ show (move) ++ ansi Reset)
    printDebug ("gameLoop: playerMove move = " ++ ansi Cyan ++ show (playerMove move) ++ ansi Reset)
    n <- playerUserInput
    printDebug ("gameLoop: n = " ++ ansi Cyan ++ show n ++ ansi Reset)
    printDebug ("gameLoop: isOccupiedAt board n = " ++ ansi Cyan ++ show (isOccupiedAt board n) ++ ansi Reset)
    if isOccupiedAt board n == True then do
        putStrLn (ansi Yellow ++ "That cell is already occupied, please pick another one." ++ ansi Reset)
        gameLoop board move (playerX, playerO)
    else do
        let newBoard :: [Cell]
            newBoard = updateCell n (playerMove move) board
        printDebug ("gameLoop: fullBoard newBoard = " ++ ansi Cyan ++ show (fullBoard newBoard) ++ ansi Reset)
        case winCheck newBoard of
            Just p -> do
                printBoard newBoard
                printDebug ("gameLoop: newBoard = " ++ ansi Cyan ++ show newBoard ++ ansi Reset)
                putStrLn (ansi Yellow ++ "Congrats!" ++ ansi Reset)
                putStrLn (ansi Yellow ++ "Player " ++ show (playerMove move) ++ " won at move " ++ show move ++ ansi Reset)
                appendFile historyFile ("\"" ++ show newBoard ++ "\"" ++ "," ++ show playerX ++ "," ++ show playerO ++ "," ++ show (playerMove move) ++ "," ++ show move ++ "\n")
            Nothing ->
                if fullBoard newBoard == True then do
                    printBoard newBoard
                    printDebug ("gameLoop: newBoard = " ++ ansi Cyan ++ show newBoard ++ ansi Reset)
                    putStrLn (ansi Yellow ++ "It's a draw!" ++ ansi Reset)
                    appendFile historyFile ("\"" ++ show newBoard ++ "\"" ++ "," ++ show playerX ++ "," ++ show playerO ++ "," ++ "" ++ "," ++ show move ++ "\n")
                else do
                    printDebug ("gameLoop: newBoard = " ++ ansi Cyan ++ show newBoard ++ ansi Reset)
                    gameLoop newBoard (move + 1) (playerX, playerO)


main :: IO ()
main = do
    historyFileExists <- doesFileExist historyFile
    if historyFileExists == True
        then
            printDebug ("main: historyFileExists = " ++ ansi Green ++ show historyFileExists ++ ansi Reset)
        else do
            printDebug ("main: historyFileExists = " ++ ansi Red ++ show historyFileExists ++ ansi Reset)
            writeFile historyFile ("Board,Player X,Player O,Winner,Moves" ++ "\n")
            printDebug ("main: " ++ ansi Green ++ "History file created" ++ ansi Reset)
    playerX <- getPlayerX
    playerO <- getPlayerO
    gameLoop (replicate 9 Empty) 1 (playerX, playerO)


{-
TODO:
- Actually use player names
- Implement different colors for each player
- Implement stats
- Rewrite functions to get player names to avoid duplicating code for both players
- Deal the buffers

DONE:
- Get player names
- Set up board
- Get user input
- Validate user input
- Create a new board for each move
- Alternate players
- Track move count
- Occupancy check
- Win conditions
- Draw conditions
- Games history
-}
