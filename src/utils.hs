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
    -- board: the sudoku board
-- Output:
    -- A Cell object
getCell :: Int -> Int -> SudokuBoard -> Cell
getCell colNo rowNo board = 
    (board !! rowNo) !! colNo
    

-- Description: 
    -- ask and return a valid command from user input
    -- if the user input cannot be parsed to a valid command
    -- call askCmd recursively until getting a valid command
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- An IO String, containing the valid command
askCmd :: String -> IO String
askCmd prompt = 
    do  putStrLn prompt
        cmd' <- getLine
        let (cmd, valid) = isValidCmd $ takeWhile (/=' ') $ dropWhile (== ' ') cmd'
        if valid then return cmd else askCmd cmd


-- Description: 
    -- ask and return a valid row index from user input,
    -- if the user input cannot be parsed to a valid row index,
    -- call askRow recursively until getting a valid row index.
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- An IO Int, containing the valid row index.
askRow :: String -> IO Int
askRow prompt =
    do  putStrLn prompt
        rowNo' <- getLine
        let (rowNo, valid) = isValidNumber 0 8 rowNo'
        if not valid then askRow "Input not accepted, try again!\nSelect a row (0-8):" else return rowNo


-- Description: 
    -- ask and return a valid column index from user input,
    -- if the user input cannot be parsed to a valid column index,
    -- call askRow recursively until getting a valid column index.
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- An IO Int, containing the valid column index.
askCol :: String -> IO Int
askCol prompt =
    do  putStrLn prompt
        colNo' <- getLine
        let (colNo, valid) = isValidNumber 0 8 colNo'
        if not valid then askCol "Input not accepted, try again!\nSelect a column (0-8):" else return colNo


-- Description: 
    -- ask and return a valid value from user input,
    -- if the user input cannot be parsed to a valid value,
    -- call askRow recursively until getting a valid value.
-- Input:
    -- prompt: the prompt information given to user
-- Output:
    -- An IO Int, containing the valid value.
askValue :: String -> IO Int
askValue prompt =
    do  putStrLn prompt
        value' <- getLine
        let (value, valid) = isValidNumber 1 9 value'
        if not valid then askValue "Input not accepted, try again!\nType in a value (1-9):" else return value


-- Description: 
    -- check if the given input is a valid value.
-- Input:
    -- lowerBound: the lower bound used for input validation
    -- upperBound: the upper bound used for input validation
    -- input: a string to be tested
-- Output:
    -- (Int, Bool) a tuple containing a valid value and a Bool value.
isValidNumber :: Int -> Int -> String -> (Int, Bool)
isValidNumber lowerBound upperBound input
    | (length input == 1) && (isDigit $ head $ input) = ((digitToInt $ head $ input), lowerBound <= (digitToInt $ head $ input) && (digitToInt $ head $ input) <= upperBound)
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
    | cmd == "help" = ("help", True)
    | containSubString cmd "load" = ("Incorrect Command, try again! Do you mean \"load\"?", False)
    | containSubString cmd "move" = ("Incorrect Command, try again! Do you mean \"move\"?, Try again!", False)
    | containSubString cmd "save" = ("Incorrect Command, try again! Do you mean \"save\"?, Try again!", False)
    | containSubString cmd "quit" = ("Incorrect Command, try again! Do you mean \"quit\"?, Try again!", False)
    | containSubString cmd "solve" = ("Incorrect Command, try again! Do you mean \"solve\"?, Try again!", False)
    | containSubString cmd "new" = ("Incorrect Command, try again! Do you mean \"new\"?, Try again!", False)
    | containSubString cmd "help" = ("Incorrect Command, try again! Do you mean \"help\"?, Try again!", False)
    | otherwise = ("\nCommand is not correct, enter \"help\" if you need help, Try again!", False)


-- Description: 
    -- Given a move and a sudoku borad, check if the move is a valid move
    -- which means to check if the move has no conflict in its row, column, and block
-- Input:
    -- colNo: the column index
    -- rowNo: the row index
    -- value: the value
    -- board: the sudoku board
-- Output:
    -- return a Bool value denoting the validation of a move on a sudoku board
isValidMove :: Int -> Int -> Int -> SudokuBoard -> Bool
isValidMove colNo rowNo value board = 
    isBlank board colNo rowNo && 
    isRowValid board rowNo value && 
    isColValid board colNo value && 
    isBlockValid board colNo rowNo value


-- Description: 
    -- update a sudoku board at a specific location, with the given value
-- Input:
    -- colNo: the column index of the board
    -- rowNo: the row index of the board
    -- newValue: the new value to be filled in
    -- board: the sudoku board
-- Output:
    -- return a new updated SudokuBoard object
updateBoard :: Int -> Int -> Int -> SudokuBoard -> SudokuBoard
updateBoard colNo rowNo newValue board = 
    map (\row -> updateRow colNo rowNo newValue row) board
    where 
        updateRow colNo rowNo newValue row = map (\cell -> updateCell colNo rowNo newValue cell) row
        updateCell colNo rowNo newValue (Cell colNo' rowNo' blockNo value)
            | colNo == colNo' && rowNo == rowNo' = Cell colNo rowNo blockNo newValue
            | otherwise = Cell colNo' rowNo' blockNo value


-- Description: 
    -- save a sudoko borad to a file
-- Input:
    -- board: a SudokuBoard object to be saved
-- Output:
    -- return An IO () object
saveToFile :: SudokuBoard -> IO ()
saveToFile = 
    safeWF . boardToFileConverter


-- Description: 
    -- Check if the game is over, or to say, complete,
    -- a game is complete if all cells are filled without any confilct.
-- Input:
    -- board: a SudokuBoard object to be tested
-- Output:
    -- Return a Bool value denoting the if the game is over.
isGameOver :: SudokuBoard -> Bool
isGameOver board = 
    isAllRowsValid && isAllColsValid && isAllBlockValid
    where 
        isAllRowsValid = isAllRowsValid' 0 board
        isAllColsValid = isAllColsValid' 0 board
        isAllBlockValid = isAllBlockValid' 0 board
        isAllRowsValid' rowNo board
            | rowNo == 9 = True
            | otherwise  = if isLineValid (board !! rowNo) then isAllRowsValid' (rowNo+1) board else False
        isAllColsValid' colNo board
            | colNo == 9 = True
            | otherwise  = if isLineValid [ row !! colNo | row <- board ] then isAllColsValid' (colNo+1) board else False
        isAllBlockValid' blockNo board
            | blockNo == 9 = True
            | otherwise    = if and [ val `elem` getBlockVals board blockNo | val <- [1..9]] then isAllBlockValid' (blockNo+1) board else False
        isLineValid line = and [ x `elem` (map (\cell -> getVal cell) line) | x <- [1..9]] 


-- Description: 
    -- Check if the cell is blank given the location and a sudoku board.
-- Input:
    -- board: a SudokuBoard object to be tested
    -- colNo: the column index of the tested location
    -- rowNo: the row index of the tested location
-- Output:
    -- Return a Bool value denoting the if cell is a blank cell.
isBlank :: SudokuBoard -> Int -> Int -> Bool
isBlank board colNo rowNo = 
    0 == getVal (getCell colNo rowNo board)


-- Description: 
    -- Check if a given value can be added to a row.
-- Input:
    -- board: a SudokuBoard object to be tested
    -- rowNo: the row index of the tested location
    -- value: the value to be added to the row
-- Output:
    -- Return a Bool value denoting the given value can be added to a row.
isRowValid :: SudokuBoard -> Int -> Int -> Bool
isRowValid board rowNo value = 
    not $ value `elem` (getRowVals board rowNo)


-- Description: 
    -- Check if a given value can be added to a column.
-- Input:
    -- board: a SudokuBoard object to be tested
    -- colNo: the column index of the tested location
    -- value: the value to be added to the column
-- Output:
    -- Return a Bool value denoting the given value can be added to a column.
isColValid :: SudokuBoard -> Int -> Int -> Bool
isColValid board colNo value = 
    not $ value `elem` (getColVals board colNo )


-- Description: 
    -- Check if a given value can be added to a block.
-- Input:
    -- board: a SudokuBoard object to be tested
    -- colNo: the column index of the tested location
    -- rowNo: the row index of the tested location
    -- value: the value to be added to the block
-- Output:
    -- Return a Bool value denoting the given value can be added to a block.
isBlockValid :: SudokuBoard -> Int -> Int -> Int -> Bool
isBlockValid board colNo rowNo value = 
    not $ value `elem` (getBlockVals board blockNo)
        where blockNo = getBlockNo $ (board !! rowNo) !! colNo


-- Description: 
    -- Get all the existed value sof a row on a sudoku board
-- Input:
    -- board: a SudokuBoard object
    -- rowNo: the row index
-- Output: 
    -- a list of Int containing the existed valus on the row
getRowVals :: SudokuBoard -> Int -> [Int]
getRowVals board rowNo = 
    [ getVal cell | cell <- board !! rowNo, getVal cell /= 0]


-- Description: 
    -- Get all the existed value sof a column on a sudoku board
-- Input:
    -- board: a SudokuBoard object
    -- colNo: the column index
-- Output: 
    -- a list of Int containing the existed valus on the column
getColVals :: SudokuBoard -> Int -> [Int]
getColVals board colNo = 
    [ getVal cell | cell <- map (\row -> row !! colNo) board, getVal cell /= 0 ]


-- Description: 
    -- Get all the existed value sof a block on a sudoku board
-- Input:
    -- board: a SudokuBoard object
    -- blockNo: the block index
-- Output: 
    -- a list of Int containing the existed valus on the block
getBlockVals :: SudokuBoard -> Int -> [Int]
getBlockVals board blockNo = [ getVal cell | cell <- concat board, (getBlockNo cell == blockNo) && (getVal cell /= 0) ] 


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
fileToBoardConverter file = board
    where   givenFile = lines file
            blockInfo = take (9) givenFile
            initialNumberInfo = drop (9) givenFile
            board = constructBoard blockInfo initialNumberInfo


-- Description: 
    -- Convert a SudokuBoard object to a file
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- a file converted from the given SudokuBoard object 
boardToFileConverter :: SudokuBoard -> [String]
boardToFileConverter board = 
    [ handleBlock row | row <- board ] ++ [ handleVals row | row <- board ]
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
getRandNum :: Int -> Int -> IO Int
getRandNum lowerBound upperBound = do
    g <- newStdGen
    return $ digitToInt $ head $ take 1 (randomRs (intToDigit lowerBound, intToDigit upperBound) g)


-- Description: 
    -- Generate a random list of Int values.
-- Input:
    -- n: the number of Int values to be generated
    -- lowerBound: the lower bound of Int values
    -- upperBound: the upper bound of Int values
    -- isDistinct: set to be True if you want to generate a list of distince Int values, False otherwise
-- Output: 
    -- IO [Int] object, [Int] is the random list of Int values generated.
getRandList :: Int -> Int -> Int -> Bool -> IO [Int]
getRandList n lowerBound upperBound isDistinct = 
    getRandList' []
    where
        getRandList' xs
            | length xs == n = return xs 
            | otherwise = do 
                num <- getRandNum lowerBound upperBound
                if isDistinct then 
                    if (num `elem` xs) then getRandList' xs else getRandList' (xs++[num])
                else getRandList' (xs++[num])


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
    -- An IO String object 
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
    -- An IO () object
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
