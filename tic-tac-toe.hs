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


-- Cells IDs
cellsids     = [1, 2, 3, 4, 5, 6, 7, 8, 9]

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
    putStrLn "Press a number from 1 to 9 to select a cell to play (numpad order)"
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
updateCell pos val xs = take (pos - 1) xs ++ [val] ++ drop pos xs


main :: IO ()
main = do

    -- Set up the initial board
    let board :: [Cell]
        board = replicate 9 Empty
    printBoard board
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "main: board = " ++ ansi Cyan ++ show board ++ ansi Reset)

    -- Get user input
    n <- playerUserInput
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "main: n = " ++ ansi Cyan ++ show n ++ ansi Reset)
    putStrLn ""

    -- Define val (Implement player rotation later)
    let val :: Cell = X
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "main: val = " ++ ansi Cyan ++ show val ++ ansi Reset)

    -- Update cells
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "main: Updating cell variables (not yet working)")
    let board1 = updateCell n val board
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "main: board1 = " ++ ansi Cyan ++ show board1 ++ ansi Reset)
    putStrLn ""

    -- Print board1
    putStrLn (ansi Magenta ++ "DEBUG: " ++ ansi Reset ++ "main: Printing updated board:")
    printBoard board1
    putStrLn ""


-- TODO:
-- - Update the cell variables based on user input, fill with an 'X' for now (currently not working)
-- - Implement check to see if the selected cell is already filled
-- - Implement player switching
-- - Implement different colors for each player
-- - Implement check for win conditions
