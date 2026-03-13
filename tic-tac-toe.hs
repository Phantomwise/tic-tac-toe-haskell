-- Tic Tac Toe


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

{-
-- Cells IDs
cellsids     = [1, 2, 3, 4, 5, 6, 7, 8, 9]
-}

-- ┏━━━┳━━━┳━━━┓
-- ┃ 7 ┃ 8 ┃ 9 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 4 ┃ 5 ┃ 6 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 1 ┃ 2 ┃ 3 ┃
-- ┗━━━┻━━━┻━━━┛


-- Mapping cells IDs to rows, columns and diagonals
row1ids, row2ids, row3ids, column1ids, column2ids, column3ids, diagonal1ids, diagonal2ids :: [Int]
row1ids      = [7, 8, 9]
row2ids      = [4, 5, 6]
row3ids      = [1, 2, 3]
column1ids   = [7, 4, 1]
column2ids   = [8, 5, 2]
column3ids   = [9, 6, 3]
diagonal1ids = [7, 5, 3]
diagonal2ids = [9, 5, 1]


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

    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "printBoard: Printing the current board:")
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
            putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "playerUserInput: Key pressed: " ++ ansi Cyan ++ [k] ++ ansi Reset)
            putStrLn (ansi Red ++ "ERROR: " ++ ansi Reset ++ "Invalid input, please try again.")
            putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "playerUserInput: User input invalid: " ++ ansi Cyan ++ show (validateUserInput k) ++ ansi Reset)
            playerUserInput
        Right n -> do
            putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "playerUserInput: Key pressed: " ++ ansi Cyan ++ [k] ++ ansi Reset)
            putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "playerUserInput: User input validated: " ++ "validateUserInput k = " ++ ansi Cyan ++ show (validateUserInput k) ++ ansi Reset)
            putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "playerUserInput: User input validated: " ++ "n = " ++ ansi Cyan ++ show n ++ ansi Reset)
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


-- Check remaining free cells
fullBoard :: [Cell] -> Bool
fullBoard board = if elem Empty board then False else True


gameLoop :: [Cell] -> Int -> IO ()
gameLoop board move = do
    printBoard board
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: board = " ++ ansi Cyan ++ show board ++ ansi Reset)
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: move = " ++ ansi Cyan ++ show (move) ++ ansi Reset)
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: playerMove move = " ++ ansi Cyan ++ show (playerMove move) ++ ansi Reset)
    n <- playerUserInput
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: n = " ++ ansi Cyan ++ show n ++ ansi Reset)
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: isOccupiedAt board n = " ++ ansi Cyan ++ show (isOccupiedAt board n) ++ ansi Reset)
    if isOccupiedAt board n == True then do
        putStrLn (ansi Yellow ++ "That cell is already occupied, please pick another one." ++ ansi Reset)
        gameLoop board move
    else do
        let newBoard :: [Cell]
            newBoard = updateCell n (playerMove move) board
        putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: fullBoard newBoard = " ++ ansi Cyan ++ show (fullBoard newBoard) ++ ansi Reset)
        if fullBoard newBoard == True then do
            printBoard newBoard
            putStrLn (ansi Yellow ++ "It's a draw!" ++ ansi Reset)
        else do
            putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "gameLoop: newBoard = " ++ ansi Cyan ++ show newBoard ++ ansi Reset)
            gameLoop newBoard (move + 1)


main :: IO ()
main = do
    gameLoop (replicate 9 Empty) 1


{-
TODO:
- Refactor the board update in a less stupid way
- Implement different colors for each player
- Implement check for win conditions

DONE:
- Set up board
- Get user input
- Validate user input
- Create a new board for each move
- Alternate players
- Track move count
- Occupancy check
-}
