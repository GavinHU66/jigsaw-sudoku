module Utils where

import System.Random
import Data.List
import Data.Char
import Control.Exception (try)
import GHC.IO.Exception (IOException(..))
import Cell

getCell :: Int -> Int -> SudokuBoard -> Cell
getCell colNo rowNo sudokuBoard = 
    (sudokuBoard !! rowNo) !! colNo
    
askCmd :: String -> IO String
askCmd prompt = 
    do  putStrLn prompt
        cmd' <- getLine
        let (cmd, valid) = isValidCmd $ takeWhile (/=' ') $ dropWhile (== ' ') cmd'
        if valid then return cmd else askCmd cmd

askRow :: String -> IO Int
askRow prompt =
    do  putStrLn prompt
        rowNo' <- getLine
        let (rowNo, valid) = isValidNumber 0 8 rowNo'
        if not valid then askRow "Input not accepted, try again!\nSelect a row (0-8):" else return rowNo

askCol :: String -> IO Int
askCol prompt =
    do  putStrLn prompt
        colNo' <- getLine
        let (colNo, valid) = isValidNumber 0 8 colNo'
        if not valid then askCol "Input not accepted, try again!\nSelect a column (0-8):" else return colNo

askValue :: String -> IO Int
askValue prompt =
    do  putStrLn prompt
        value' <- getLine
        let (value, valid) = isValidNumber 1 9 value'
        if not valid then askValue "Input not accepted, try again!\nType in a value (1-9):" else return value

isValidNumber :: (Ord a1, Ord a2, Num a3, Num a1, Num a2) => a1 -> a2 -> [Char] -> (a3, Bool)
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
    | otherwise = ("Command is not correct, enter \"help\" if you need help, Try again!", False)

isValidMove :: Int -> Int -> Int -> SudokuBoard -> Bool
isValidMove colNo rowNo value sudokuBoard = 
    isBlank sudokuBoard colNo rowNo && 
    isRowValid sudokuBoard rowNo value && 
    isColValid sudokuBoard colNo value && 
    isBlockValid sudokuBoard colNo rowNo value

updateBoard :: Int -> Int -> Int -> SudokuBoard -> SudokuBoard
updateBoard tarColNo tarRowNo newValue sudokuBoard = 
    map (\row -> updateRow tarColNo tarRowNo newValue row) sudokuBoard
    where 
        updateRow tarColNo tarRowNo newValue row = map (\cell -> updateCell tarColNo tarRowNo newValue cell) row
        updateCell tarColNo tarRowNo newValue (Cell colNo rowNo blockNo value)
            | tarColNo == colNo && tarRowNo == rowNo = Cell tarColNo tarRowNo blockNo newValue
            | otherwise = Cell colNo rowNo blockNo value

saveToFile :: SudokuBoard -> IO ()
saveToFile sudokuBoard = 
    safeWF content
    where content = boardToFileConverter sudokuBoard

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
            | otherwise    = if allTrue [ val `elem` getBlockVals sudokuBoard blockNo | val <- [1..9]] then isAllBlockValid' (blockNo+1) sudokuBoard else False
        isLineValid line = allTrue [ x `elem` (map (\cell -> getVal cell) line) | x <- [1..9]] 

isBlank :: SudokuBoard -> Int -> Int -> Bool
isBlank sudokuBoard colNo rowNo = 
    0 == getVal (getCell colNo rowNo sudokuBoard)

isRowValid :: SudokuBoard -> Int -> Int -> Bool
isRowValid sudokuBoard rowNo value = 
    not $ value `elem` (getRowVals sudokuBoard rowNo)

isColValid :: SudokuBoard -> Int -> Int -> Bool
isColValid sudokuBoard colNo value = 
    not $ value `elem` (getColVals sudokuBoard colNo )

isBlockValid :: SudokuBoard -> Int -> Int -> Int -> Bool
isBlockValid sudokuBoard colNo rowNo value = 
    not $ value `elem` (getBlockVals sudokuBoard blockNo)
        where blockNo = getBlockNo $ (sudokuBoard !! rowNo) !! colNo

getRowVals :: SudokuBoard -> Int -> [Int]
getRowVals sudokuBoard rowNo = 
    [ getVal cell | cell <- sudokuBoard !! rowNo, getVal cell /= 0]

getColVals :: SudokuBoard -> Int -> [Int]
getColVals sudokuBoard colNo = 
    [ getVal cell | cell <- map (\row -> row !! colNo) sudokuBoard, getVal cell /= 0 ]

getBlockVals :: SudokuBoard -> Int -> [Int]
getBlockVals sudokuBoard blockNo = [ getVal cell | cell <- concat sudokuBoard, (getBlockNo cell == blockNo) && (getVal cell /= 0) ] 

allTrue :: [Bool] -> Bool
allTrue xs
    | length xs == 0 = True
    | length xs == 1 = xs !! 0
    | otherwise      = if xs !! 0 then allTrue (tail xs) else False

containSubString :: String -> String -> Bool
containSubString wholeStr subStr
    | length wholeStr < length subStr = False 
    | subStr `isPrefixOf` wholeStr    = True
    | otherwise                       = containSubString (tail wholeStr) subStr

fileToBoardConverter :: String -> SudokuBoard
fileToBoardConverter file = sudokuBoard
    where   givenFile = lines file
            blockInfo = take (9) givenFile
            initialNumberInfo = drop (9) givenFile
            sudokuBoard = constructBoard blockInfo initialNumberInfo

boardToFileConverter :: SudokuBoard -> [String]
boardToFileConverter sudokuBoard = 
    [ handleBlock row | row <- sudokuBoard ] ++ [ handleVals row | row <- sudokuBoard ]
    where 
        handleVals row = map (\cell -> if getVal cell == 0 then '.' else intToDigit $ getVal $ cell) row
        handleBlock row = map (\cell -> intToDigit $ getBlockNo $ cell) row

constructBoard :: [String] -> [String] -> SudokuBoard
constructBoard blockInfo cellInfo = 
    map (\(rowNo, blockInfoRow) -> constructRow rowNo blockInfoRow cellInfo) $ zip [0..] blockInfo
        where 
            constructRow rowNo blockInfoRow cellInfo = map (\(colNo, blockNo) -> Cell colNo rowNo (digitToInt blockNo) (valueToInt ((cellInfo !! rowNo) !! colNo)) ) $ zip [0..] blockInfoRow
            valueToInt value
                | value == '.' = 0
                | otherwise = digitToInt value

generateRandomNumBetween :: Char -> Char -> IO Int
generateRandomNumBetween lowerBound upperBound = do
    g <- newStdGen
    return $ digitToInt $ head $ take 1 (randomRs (lowerBound, upperBound) g)

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

rExceptionHandler :: Either IOError a -> IO Bool 
rExceptionHandler (Right _) = do putStrLn "\nfile loaded successfully!"; return True
rExceptionHandler (Left e) = do printErr e; return False

wExceptionHandler :: Either IOError a -> IO Bool 
wExceptionHandler (Right _) = return True
wExceptionHandler (Left e) = do printErr e; return False

printErr :: IOException -> IO ()
printErr e = 
    putStrLn $ concat [ 
        "\nOpps! There is something wrong, filename = ", 
        show $ getJust $ ioe_filename e, ", error message = ", 
        show $ ioe_description e]
         
safeRF :: IO String
safeRF = do 
    putStrLn "\nPlease indicate the file name to be loaded:\ne.g., \"./YourPath/map.txt\", or file name if it\'s in the same folder";
    filePath' <- getLine
    let filePath = takeWhile (/=' ') $ dropWhile (== ' ') filePath'
    valid <- try (readFile filePath) >>= rExceptionHandler
    if valid then do file <- readFile filePath; return file else safeRF

safeWF :: [String] -> IO ()
safeWF content = do 
    putStrLn "\nPlease indicate the file name to be saved"
    filePath' <- getLine
    let filePath = takeWhile (/=' ') $ dropWhile (== ' ') filePath'
    valid <- try (writeFile filePath (unlines content)) >>= wExceptionHandler
    if valid then do writeFile filePath (unlines content); putStrLn ("Saved successfully to file "++filePath++"!") else safeWF content

getJust :: Maybe a -> a
getJust (Just a) = a