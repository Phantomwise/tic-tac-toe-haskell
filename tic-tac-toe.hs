-- Tic Tac Toe

-- Define color codes
red     = "\ESC[0;31m"
green   = "\ESC[0;32m"
yellow  = "\ESC[0;33m"
blue    = "\ESC[0;34m"
magenta = "\ESC[0;35m"
cyan    = "\ESC[0;36m"
reset   = "\ESC[0m"

-- Define a new type for the content of the cells
data Cell = Empty | X | O
    deriving (Eq)
    -- deriving (Eq, Show)

instance Show Cell where
    show Empty = "·"
    show X = "X"
    show O = "O"

-- Board cell ids:
-- ┏━━━┳━━━┳━━━┓
-- ┃ 7 ┃ 8 ┃ 9 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 4 ┃ 5 ┃ 6 ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ 1 ┃ 2 ┃ 3 ┃
-- ┗━━━┻━━━┻━━━┛

-- Cells IDs
cellsids     = [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- Mapping cells IDs to rows, columns and diagonals
row1ids      = [7, 8, 9]
row2ids      = [4, 5, 6]
row3ids      = [1, 2, 3]
column1ids   = [7, 4, 1]
column2ids   = [8, 5, 2]
column3ids   = [9, 6, 3]
diagonal1ids = [7, 5, 3]
diagonal2ids = [9, 5, 1]

-- Empty board:
-- ┏━━━┳━━━┳━━━┓
-- ┃ · ┃ · ┃ · ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ · ┃ · ┃ · ┃
-- ┣━━━╋━━━╋━━━┫
-- ┃ · ┃ · ┃ · ┃
-- ┗━━━┻━━━┻━━━┛

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

-- Function to validate user input and convert it to an Int, alternative version
-- import Data.Char (digitToInt)
-- validateUserInput :: Char -> Either String Int
-- validateUserInput k
--   | k >= '1' && k <= '9' = Right (digitToInt k)
--   | otherwise             = Left "Invalid input. Please enter a number from 1 to 9."
-- I kinda like the other version better, keeping the alt for reference

main :: IO ()
main = do

    -- Set up cells
    let cell1 :: Cell
        cell1 = Empty
        cell2 :: Cell
        cell2 = Empty
        cell3 :: Cell
        cell3 = Empty
        cell4 :: Cell
        cell4 = Empty
        cell5 :: Cell
        cell5 = Empty
        cell6 :: Cell
        cell6 = Empty
        cell7 :: Cell
        cell7 = Empty
        cell8 :: Cell
        cell8 = Empty
        cell9 :: Cell
        cell9 = Empty

    -- Bindings for printing the board
    let printRow1 :: String
        printRow1 = "┃ " ++ show cell7 ++ " ┃ " ++ show cell8 ++ " ┃ " ++ show cell9 ++ " ┃"
        printRow2 :: String
        printRow2 = "┃ " ++ show cell4 ++ " ┃ " ++ show cell5 ++ " ┃ " ++ show cell6 ++ " ┃"
        printRow3 :: String
        printRow3 = "┃ " ++ show cell1 ++ " ┃ " ++ show cell2 ++ " ┃ " ++ show cell3 ++ " ┃"

    -- Function to print the updated board
    let printBoard :: IO ()
        printBoard = do
             putStrLn "┏━━━┳━━━┳━━━┓"
             putStrLn printRow1
             putStrLn "┣━━━╋━━━╋━━━┫"
             putStrLn printRow2
             putStrLn "┣━━━╋━━━╋━━━┫"
             putStrLn printRow3
             putStrLn "┗━━━┻━━━┻━━━┛"
             putStrLn ""

    -- Print updated board
    putStrLn (magenta ++ "DEBUG: " ++ reset ++ "Main: Printing starting board (should be empty):")
    printBoard
    putStrLn ""

    -- Get user input
    n <- playerUserInput
    putStrLn (magenta ++ "DEBUG: " ++ reset ++ "Main: User selected cell: " ++ show n)
    putStrLn ""

    -- Updated cells
    putStrLn (magenta ++ "DEBUG: " ++ reset ++ "Main: Updating cell variables (not yet working)")
    putStrLn ""

    -- Print updated board
    putStrLn (magenta ++ "DEBUG: " ++ reset ++ "Main: Printing updated board:")
    printBoard
    putStrLn ""


-- Function to get user input and validate it
playerUserInput :: IO Int
playerUserInput = do
    putStrLn "Press a number from 1 to 9 to select a cell to play (numpad order)"
    k <- getChar
    _ <- getLine -- Trash everything after the first character and be mad at having to deal with newlines and buffers >_<
    case validateUserInput k of
        Left err -> do
            putStrLn (magenta ++ "DEBUG: " ++ reset ++ "playerUserInput: Key pressed: " ++ [k])
            putStrLn (red ++ "ERROR: " ++ reset ++ "playerUserInput: User input invalid: " ++ show (validateUserInput k))
            playerUserInput
        Right n -> do
            putStrLn (magenta ++ "DEBUG: " ++ reset ++ "playerUserInput: Key pressed: " ++ [k])
            putStrLn (magenta ++ "DEBUG: " ++ reset ++ "playerUserInput: User input validated: validateUserInput k = " ++ show (validateUserInput k))
            putStrLn (magenta ++ "DEBUG: " ++ reset ++ "playerUserInput: User input validated: n = " ++ show n)
            putStrLn ""
            return n


-- TODO:
-- - Update the cell variables based on user input, fill with an 'X' for now (currently not working)
-- - Implement check to see if the selected cell is already filled
-- - Implement player switching
-- - Implement win condition checking
