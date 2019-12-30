import System.Random
import Data.Char
import Data.List
import RenderBoard
import Cell
import Utils


main :: IO ()
main = do
    putStrLn "please enter file path (e.g., \"./YourPath/map.txt\", or file name if it\'s in the same folder): "
    filePath <- getLine
    file <- readFile filePath
    let sudokuBoard = fileToBoardConverter file
        file2 = boardToFileConverter sudokuBoard
    putStrLn $ unlines $ file2

    -- randomSudokuBoard <- generateRandomSudokoBoard sudokuBoard
    -- putStrLn "Original Board:"
    -- printBoard $ sudokuBoard
    -- putStrLn "New Board:"
    -- printBoard $ randomSudokuBoard

generateRandomSudokoBoard sudokuBoard = do
    sudokuBoard' <- generateRandomCompletedSudokoBoard sudokuBoard
    generateSudokoBoardWithCellsRemoved sudokuBoard' 10

generateRandomCompletedSudokoBoard sudokuBoard = do 
    randomList <- generateRandomList
    return $ updateBoardByRandomList sudokuBoard randomList

updateBoardByRandomList sudokuBoard randomList = 
    map (\row -> updateRow row) sudokuBoard
    where 
        updateRow row = map (\cell -> updateCell cell) row
        updateCell (Cell colNo rowNo blockNo value) = Cell colNo rowNo blockNo (randomList !! (value-1))
    
generateSudokoBoardWithCellsRemoved sudokuBoard n = do
    cellsToBeRemoved <- generateRandomCoords n
    return $ updateBoardByEmptyCells sudokuBoard cellsToBeRemoved

updateBoardByEmptyCells sudokuBoard cellsToBeRemoved = do
    map (\row -> updateRow row) sudokuBoard
    where 
        updateRow row = map (\cell -> updateCell cell) row
        updateCell (Cell colNo rowNo blockNo value)
            | (colNo, rowNo) `elem` cellsToBeRemoved = Cell colNo rowNo blockNo 0
            | otherwise = Cell colNo rowNo blockNo value
            
generateRandomCoords n = 
    generateRandomCoords' n []
    where 
        generateRandomCoords' n xs
            | length xs == n = return xs
            | otherwise = do
                colNo <- generateRandomNumBetween '0' '8'
                rowNo <- generateRandomNumBetween '0' '8'
                let coord = (colNo, rowNo)
                if (coord `elem` xs) then generateRandomCoords' n xs else generateRandomCoords' n (xs++[coord])

generateRandomList :: IO [Int]
generateRandomList = 
    generateRandomList' []
    where
        generateRandomList' xs
            | length xs == 9 = return xs 
            | otherwise = do 
                num <- generateRandomNumBetween '1' '9'
                if (num `elem` xs) then generateRandomList' xs else generateRandomList' (xs++[num])

generateRandomNumBetween :: Char -> Char -> IO Int
generateRandomNumBetween lowerBound upperBound = do
    g <- newStdGen
    let x = take 1 (randomRs (lowerBound, upperBound) g)
    return $ digitToInt $ head x 




updateBoard tarColNo tarRowNo newValue sudokuBoard = 
    map (\row -> updateRow tarColNo tarRowNo newValue row) sudokuBoard
    where 
        updateRow tarColNo tarRowNo newValue row = map (\cell -> updateCell tarColNo tarRowNo newValue cell) row
        updateCell tarColNo tarRowNo newValue (Cell colNo rowNo blockNo value)
            | tarColNo == colNo && tarRowNo == rowNo = Cell tarColNo tarRowNo blockNo newValue
            | otherwise = Cell colNo rowNo blockNo value

            


-- solve :: Int -> [String] -> [String] -> [String] -> [String]
-- solve 81 column block board = board
-- solve n column block board | retrieveBoard n board /= '.' = solve (n+1) column block board
--                            | and [not $ check n (intToDigit i) column block board| i <- [1..9]] = []
--                            | otherwise = firstNotEmpty [if check n (intToDigit i) column block board then solve (n+1) (setList ((n `mod` 9)+1) (intToDigit i) column) (setList (1 + (digitToInt $ retrieve ((n `mod` 9)+1) $ retrieve ((n `div` 9)+1) board)) (intToDigit i) block) $ setBoardValue n (intToDigit i) board else [] | i <-[1..9]]