module Utils where

import System.Random
import Data.List
import Data.Char
import Control.Exception (try)
import GHC.IO.Exception (IOException(..))
import Cell


-- Description: 
    -- Given a column index and a row index, 
    -- get a cell object from a SudokuBoard object.
-- Input:
    -- colNo: the column index
    -- rowNo: the row index
    -- sudokuBoard: the sudoku board
-- Output:
    -- a cell object
getCell :: Int -> Int -> SudokuBoard -> Cell
getCell colNo rowNo sudokuBoard = 
    (sudokuBoard !! rowNo) !! colNo
    

-- Description: 
    -- ask and return a valid command from user input
    -- if the user input cannot be parsed to a valid command
    -- call askCmd recursively until getting a valid command
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- a IO String, containing the valid command
askCmd :: String -> IO String
askCmd prompt = 
    do  putStrLn prompt
        cmd' <- getLine
        let (cmd, valid) = isValidCmd $ takeWhile (/=' ') $ dropWhile (== ' ') cmd'
        if valid then return cmd else askCmd cmd


-- Description: 
    -- ask and return a valid row index from user input
    -- if the user input cannot be parsed to a valid row index
    -- call askRow recursively until getting a valid row index
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- a IO Int, containing the valid row index
askRow :: String -> IO Int
askRow prompt =
    do  putStrLn prompt
        rowNo' <- getLine
        let (rowNo, valid) = isValidNumber 0 8 rowNo'
        if not valid then askRow "Input not accepted, try again!\nSelect a row (0-8):" else return rowNo


-- Description: 
    -- ask and return a valid column index from user input
    -- if the user input cannot be parsed to a valid column index
    -- call askRow recursively until getting a valid column index
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- a IO Int, containing the valid column index
askCol :: String -> IO Int
askCol prompt =
    do  putStrLn prompt
        colNo' <- getLine
        let (colNo, valid) = isValidNumber 0 8 colNo'
        if not valid then askCol "Input not accepted, try again!\nSelect a column (0-8):" else return colNo


-- Description: 
    -- ask and return a valid value from user input
    -- if the user input cannot be parsed to a valid value
    -- call askRow recursively until getting a valid value
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- a IO Int, containing the valid value
askValue :: String -> IO Int
askValue prompt =
    do  putStrLn prompt
        value' <- getLine
        let (value, valid) = isValidNumber 1 9 value'
        if not valid then askValue "Input not accepted, try again!\nType in a value (1-9):" else return value


-- Description: 
    -- check if the given input is a valid value
-- Input:
    -- lowerBound: the lower bound used for input validation
    -- upperBound: the upper bound used for input validation
    -- input: a string to be tested
-- Output:
    -- (Int, Bool) a tuple containing a valid value and a Bool value
isValidNumber :: Int -> Int -> String -> (Int, Bool)
isValidNumber lowerBound upperBound input
    | input == "0" = (0, lowerBound <= 0 && 0 <= upperBound)
    | input == "1" = (1, lowerBound <= 1 && 1 <= upperBound)
    | input == "2" = (2, lowerBound <= 2 && 2 <= upperBound)
    | input == "3" = (3, lowerBound <= 3 && 3 <= upperBound)
    | input == "4" = (4, lowerBound <= 4 && 4 <= upperBound)
    | input == "5" = (5, lowerBound <= 5 && 5 <= upperBound)
    | input == "6" = (6, lowerBound <= 6 && 6 <= upperBound)
    | input == "7" = (7, lowerBound <= 7 && 7 <= upperBound)
    | input == "8" = (8, lowerBound <= 8 && 8 <= upperBound)
    | input == "9" = (9, lowerBound <= 9 && 9 <= upperBound)
    | input == "quit" = (-1, True)
    | otherwise = (-1, False)


-- Description: 
    -- check if the given input is a valid command
-- Input:
    -- cmd: a string containing a command to be tested
-- Output:
    -- (Int, Bool) a tuple containing a valid command and a Bool value
isValidCmd :: String -> (String, Bool)
isValidCmd cmd
    | cmd == "load" = ("load", True)
    | cmd == "move" = ("move", True)
    | cmd == "save" = ("save", True)
    | cmd == "quit" = ("quit", True)
    | cmd == "solve" = ("solve", True)
    | cmd == "new" = ("new", True)
    | containSubString cmd "load" = ("Incorrect Command, try again! Do you mean \"load\"?", False)
    | containSubString cmd "move" = ("Incorrect Command, try again! Do you mean \"move\"?, Try again!", False)
    | containSubString cmd "save" = ("Incorrect Command, try again! Do you mean \"save\"?, Try again!", False)
    | containSubString cmd "quit" = ("Incorrect Command, try again! Do you mean \"quit\"?, Try again!", False)
    | containSubString cmd "solve" = ("Incorrect Command, try again! Do you mean \"solve\"?, Try again!", False)
    | containSubString cmd "new" = ("Incorrect Command, try again! Do you mean \"new\"?, Try again!", False)
    | otherwise = ("\nCommand is not correct, enter \"help\" if you need help, Try again!", False)

-- Description: 
    -- Given a move and a sudoku borad, check if the move is a valid move
    -- which means to check if the move has no conflict in its row, column, and block
-- Input:
    -- colNo: the column index
    -- rowNo: the row index
    -- value: the value
    -- sudokuBoard: the sudoku board
-- Output:
    -- return a Bool value denoting the validation of a move on a sudoku board
isValidMove :: Int -> Int -> Int -> SudokuBoard -> Bool
isValidMove colNo rowNo value sudokuBoard = 
    isBlank sudokuBoard colNo rowNo && 
    isRowValid sudokuBoard rowNo value && 
    isColValid sudokuBoard colNo value && 
    isBlockValid sudokuBoard colNo rowNo value

-- Description: 
    -- update a sudoku board at a specific location, with the given value
-- Input:
    -- tarColNo: the column index of the board
    -- tarRowNo: the row index of the board
    -- newValue: the new value to be filled in
    -- sudokuBoard: the sudoku board
-- Output:
    -- return a new updated SudokuBoard object
updateBoard :: Int -> Int -> Int -> SudokuBoard -> SudokuBoard
updateBoard tarColNo tarRowNo newValue sudokuBoard = 
    map (\row -> updateRow tarColNo tarRowNo newValue row) sudokuBoard
    where 
        updateRow tarColNo tarRowNo newValue row = map (\cell -> updateCell tarColNo tarRowNo newValue cell) row
        updateCell tarColNo tarRowNo newValue (Cell colNo rowNo blockNo value)
            | tarColNo == colNo && tarRowNo == rowNo = Cell tarColNo tarRowNo blockNo newValue
            | otherwise = Cell colNo rowNo blockNo value

-- Description: 
    -- save a sudoko borad to a file
-- Input:
    -- sudokuBoard: a SudokuBoard object to be saved
-- Output:
    -- return a IO () object
saveToFile :: SudokuBoard -> IO ()
saveToFile sudokuBoard = 
    safeWF content
    where content = boardToFileConverter sudokuBoard

-- Description: 
    -- Check if the game is over, or to say, complete,
    -- a game is complete if all cells are filled without any confilct.
-- Input:
    -- sudokuBoard: a SudokuBoard object to be tested
-- Output:
    -- Return a Bool value denoting the if the game is over.
isGameOver :: SudokuBoard -> Bool
isGameOver sudokuBoard = 
    isAllRowsValid && isAllColsValid && isAllBlockValid
    where 
        isAllRowsValid = isAllRowsValid' 0 sudokuBoard
        isAllColsValid = isAllColsValid' 0 sudokuBoard
        isAllBlockValid = isAllBlockValid' 0 sudokuBoard
        isAllRowsValid' rowNo sudokuBoard
            | rowNo == 9 = True
            | otherwise  = if isLineValid (sudokuBoard !! rowNo) then isAllRowsValid' (rowNo+1) sudokuBoard else False
        isAllColsValid' colNo sudokuBoard
            | colNo == 9 = True
            | otherwise  = if isLineValid [ row !! colNo | row <- sudokuBoard ] then isAllColsValid' (colNo+1) sudokuBoard else False
        isAllBlockValid' blockNo sudokuBoard
            | blockNo == 9 = True
            | otherwise    = if and [ val `elem` getBlockVals sudokuBoard blockNo | val <- [1..9]] then isAllBlockValid' (blockNo+1) sudokuBoard else False
        isLineValid line = and [ x `elem` (map (\cell -> getVal cell) line) | x <- [1..9]] 

-- Description: 
    -- Check if the cell is blank given the location and a sudoku board.
-- Input:
    -- sudokuBoard: a SudokuBoard object to be tested
    -- colNo: the column index of the tested location
    -- rowNo: the row index of the tested location
-- Output:
    -- Return a Bool value denoting the if cell is a blank cell.
isBlank :: SudokuBoard -> Int -> Int -> Bool
isBlank sudokuBoard colNo rowNo = 
    0 == getVal (getCell colNo rowNo sudokuBoard)

-- Description: 
    -- Check if a given value can be added to a row.
-- Input:
    -- sudokuBoard: a SudokuBoard object to be tested
    -- rowNo: the row index of the tested location
    -- value: the value to be added to the row
-- Output:
    -- Return a Bool value denoting the given value can be added to a row.
isRowValid :: SudokuBoard -> Int -> Int -> Bool
isRowValid sudokuBoard rowNo value = 
    not $ value `elem` (getRowVals sudokuBoard rowNo)

-- Description: 
    -- Check if a given value can be added to a column.
-- Input:
    -- sudokuBoard: a SudokuBoard object to be tested
    -- colNo: the column index of the tested location
    -- value: the value to be added to the column
-- Output:
    -- Return a Bool value denoting the given value can be added to a column.
isColValid :: SudokuBoard -> Int -> Int -> Bool
isColValid sudokuBoard colNo value = 
    not $ value `elem` (getColVals sudokuBoard colNo )

-- Description: 
    -- Check if a given value can be added to a block.
-- Input:
    -- sudokuBoard: a SudokuBoard object to be tested
    -- colNo: the column index of the tested location
    -- rowNo: the row index of the tested location
    -- value: the value to be added to the block
-- Output:
    -- Return a Bool value denoting the given value can be added to a block.
isBlockValid :: SudokuBoard -> Int -> Int -> Int -> Bool
isBlockValid sudokuBoard colNo rowNo value = 
    not $ value `elem` (getBlockVals sudokuBoard blockNo)
        where blockNo = getBlockNo $ (sudokuBoard !! rowNo) !! colNo

-- Description: 
    -- Get all the existed value sof a row on a sudoku board
-- Input:
    -- sudokuBoard: a SudokuBoard object
    -- rowNo: the row index
-- Output: 
    -- a list of Int containing the existed valus on the row
getRowVals :: SudokuBoard -> Int -> [Int]
getRowVals sudokuBoard rowNo = 
    [ getVal cell | cell <- sudokuBoard !! rowNo, getVal cell /= 0]

-- Description: 
    -- Get all the existed value sof a column on a sudoku board
-- Input:
    -- sudokuBoard: a SudokuBoard object
    -- colNo: the column index
-- Output: 
    -- a list of Int containing the existed valus on the column
getColVals :: SudokuBoard -> Int -> [Int]
getColVals sudokuBoard colNo = 
    [ getVal cell | cell <- map (\row -> row !! colNo) sudokuBoard, getVal cell /= 0 ]

-- Description: 
    -- Get all the existed value sof a block on a sudoku board
-- Input:
    -- sudokuBoard: a SudokuBoard object
    -- blockNo: the block index
-- Output: 
    -- a list of Int containing the existed valus on the block
getBlockVals :: SudokuBoard -> Int -> [Int]
getBlockVals sudokuBoard blockNo = [ getVal cell | cell <- concat sudokuBoard, (getBlockNo cell == blockNo) && (getVal cell /= 0) ] 

-- Description: 
    -- To check if a given string is a substring of annother given string.
-- Input:
    -- wholeStr: the string
    -- subStr: the substring
-- Output: 
    -- Returns True if a given string is a substring of annother given string, False otherwise
containSubString :: String -> String -> Bool
containSubString wholeStr subStr
    | length wholeStr < length subStr = False 
    | subStr `isPrefixOf` wholeStr    = True
    | otherwise                       = containSubString (tail wholeStr) subStr

-- Description: 
    -- Convert a file to a SudokuBoard object
-- Input:
    -- file: the file to be converted
-- Output: 
    -- a SudokuBoard object converted from the given file
fileToBoardConverter :: String -> SudokuBoard
fileToBoardConverter file = sudokuBoard
    where   givenFile = lines file
            blockInfo = take (9) givenFile
            initialNumberInfo = drop (9) givenFile
            sudokuBoard = constructBoard blockInfo initialNumberInfo

-- Description: 
    -- Convert a SudokuBoard object to a file
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- a file converted from the given SudokuBoard object 
boardToFileConverter :: SudokuBoard -> [String]
boardToFileConverter sudokuBoard = 
    [ handleBlock row | row <- sudokuBoard ] ++ [ handleVals row | row <- sudokuBoard ]
    where 
        handleVals row = map (\cell -> if getVal cell == 0 then '.' else intToDigit $ getVal $ cell) row
        handleBlock row = map (\cell -> intToDigit $ getBlockNo $ cell) row

-- Description: 
    -- Construct a SudokuBoard object from the given block information and cell value information
-- Input:
    -- blockInfo: the information of block in a file
    -- cellInfo: the information of cell value in a file
-- Output: 
    -- a SudokuBoard object constructed from the given block information and cell value information
constructBoard :: [String] -> [String] -> SudokuBoard
constructBoard blockInfo cellInfo = 
    map (\(rowNo, blockInfoRow) -> constructRow rowNo blockInfoRow cellInfo) $ zip [0..] blockInfo
        where 
            constructRow rowNo blockInfoRow cellInfo = map (\(colNo, blockNo) -> Cell colNo rowNo (digitToInt blockNo) (valueToInt ((cellInfo !! rowNo) !! colNo)) ) $ zip [0..] blockInfoRow
            valueToInt value
                | value == '.' = 0
                | otherwise = digitToInt value

-- Description: 
    -- Generate a random Int value.
-- Input:
    -- lowerBound: the lower bound of Int value
    -- upperBound: the upper bound of Int value
-- Output: 
    -- IO Int object, Int is the random Int values generated.
generateRandomNumBetween :: Char -> Char -> IO Int
generateRandomNumBetween lowerBound upperBound = do
    g <- newStdGen
    return $ digitToInt $ head $ take 1 (randomRs (lowerBound, upperBound) g)

-- Description: 
    -- Generate a random list of Int values.
-- Input:
    -- n: the number of Int values to be generated
    -- lowerBound: the lower bound of Int values
    -- upperBound: the upper bound of Int values
    -- isDistinct: set to be True if you want to generate a list of distince Int values, False otherwise
-- Output: 
    -- IO [Int] object, [Int] is the random list of Int values generated.
generateRandomList :: Int -> Char -> Char -> Bool -> IO [Int]
generateRandomList n lowerBound upperBound isDistinct = 
    generateRandomList' []
    where
        generateRandomList' xs
            | length xs == n = return xs 
            | otherwise = do 
                num <- generateRandomNumBetween lowerBound upperBound
                if isDistinct then 
                    if (num `elem` xs) then generateRandomList' xs else generateRandomList' (xs++[num])
                else generateRandomList' (xs++[num])

-- Description: 
    -- Exception Handler for a read file operation
-- Output: 
    -- IO Bool, True if no exception, False otherwise
rExceptionHandler :: Either IOError a -> IO Bool 
rExceptionHandler (Right _) = do putStrLn "\nfile loaded successfully!"; return True
rExceptionHandler (Left e) = do printErr e; return False

-- Description: 
    -- Exception Handler for a write file operation
-- Output: 
    -- IO Bool, True if no exception, False otherwise
wExceptionHandler :: Either IOError a -> IO Bool 
wExceptionHandler (Right _) = return True
wExceptionHandler (Left e) = do printErr e; return False

-- Description: 
    -- Convert a SudokuBoard object to a file
printErr :: IOException -> IO ()
printErr e = 
    putStrLn $ concat [ 
        "\nOpps! There is something wrong, filename = ", 
        show $ getJust $ ioe_filename e, ", error message = ", 
        show $ ioe_description e]

-- Description: 
    -- A safe read file function, it handles the exception
    -- happens during reading from a file.
-- Output: 
    -- a IO String object 
safeRF :: IO String
safeRF = do 
    putStrLn "\nPlease indicate the file name, e.g., \"sample.txt\", to be loaded:";
    filePath' <- getLine
    let filePath = "../maps/" ++ (takeWhile (/=' ') $ dropWhile (== ' ') filePath')
    valid <- try (readFile filePath) >>= rExceptionHandler
    if valid then do file <- readFile filePath; return file else safeRF

-- Description: 
    -- A safe write file function, it handles the exception
    -- happens during writing to a file.
-- Input:
    -- content: the file content to be written
-- Output: 
    -- a IO () object
safeWF :: [String] -> IO ()
safeWF content = do 
    putStrLn "\nPlease indicate the file name to be saved"
    filePath' <- getLine
    let filePath = "../maps/" ++ (takeWhile (/=' ') $ dropWhile (== ' ') filePath')
    valid <- try (writeFile filePath (unlines content)) >>= wExceptionHandler
    if valid then do writeFile filePath (unlines content); putStrLn ("Saved successfully to file "++filePath++"!") else safeWF content

-- Description: 
    -- Get the value from a (Just a)
-- Input:
    -- Just a
-- Output: 
    -- a of (Jast a)
getJust :: Maybe a -> a
getJust (Just a) = a