module Generator where

import System.Random
import Data.Char
import Cell
import RenderBoard
import Utils
import Solver

{-
step1 = [
    [((3,0),(3,1),0),((3,0),(4,0),0),((3,1),(4,1),0)],
    [((6,0),(7,0),1),((6,0),(6,1),1),((6,1),(7,1),1)],
    [((7,3),(8,3),2),((7,3),(7,4),2),((8,3),(8,4),2)],
    [((7,6),(8,6),5),((7,6),(7,7),5),((8,6),(8,7),5)],
    [((4,7),(5,7),8),((4,8),(5,8),8),((5,7),(5,8),8)],
    [((1,7),(2,7),7),((1,8),(2,8),7),((2,7),(2,8),7)],
    [((0,5),(1,5),6),((0,4),(0,5),6),((1,4),(1,5),6)],
    [((0,2),(1,2),3),((0,1),(0,2),3),((1,1),(1,2),3)]]
centralStep1 = [
    [[(3,2,4),(5,3,1)],[(5,6,4),(3,5,7)]],
    [[(6,3,4),(5,5,5)],[(2,5,4),(3,3,3)]]]

step2 = [
    [((0,3),(1,3),0),((0,3),(0,4),0),((1,3),(1,4),0)],
    [((0,6),(1,6),3),((0,6),(0,7),3),((1,6),(1,7),3)],
    [((3,7),(3,8),6),((3,7),(4,7),6),((3,8),(4,8),6)],
    [((6,7),(6,8),7),((6,7),(7,7),7),((6,8),(7,8),7)],
    [((7,5),(8,5),8),((7,4),(7,5),8),((8,4),(8,5),8)],
    [((7,2),(8,2),5),((7,1),(7,2),5),((8,1),(8,2),5)],
    [((5,0),(5,1),2),((4,0),(5,0),2),((4,1),(5,1),2)],
    [((2,0),(2,1),1),((1,0),(2,0),1),((2,1),(1,1),1)]]
centralStep2 = [
    [[(5,2,4),(3,3,1)],[(3,6,4),(5,5,7)]],
    [[(2,3,4),(3,5,3)],[(6,5,4),(5,3,5)]]]

generateRandomSudokoBoard :: Int -> IO SudokuBoard
generateRandomSudokoBoard n = do
    file <- readFile "map6.txt"
    direction <- generateRandomNumBetween '0' '1'
    choicesIdx <- generateRandomList 8 '0' '2' False
    centralIdx <- generateRandomNumBetween '0' '1'
    centralIdx' <- generateRandomNumBetween '0' '2'
    let sudokuBoard = fileToBoardConverter file
        step = if direction == 0 then step1 else step2
        centralStep = if direction == 0 then centralStep1 else centralStep2
        centralMove = if centralIdx' == 2 then concat (centralStep !! centralIdx) else (centralStep !! centralIdx) !! centralIdx'
        sequence = [ choice !! idx | (choice, idx) <- zip step choicesIdx ]
        sequence' = concat [ [(c1,r1,b), (c2,r2,b)] | ((c1,r1),(c2,r2),b) <- sequence ] ++ centralMove
        emptySodukoBoard = updateBoardBlocks sudokuBoard sequence'
    newSodukoBoard <- fillEmptyBoard n emptySodukoBoard

    return newSodukoBoard
    -- let (hasAnswer, board) = mySolver newSodukoBoard
    -- if hasAnswer then return newSodukoBoard else generateRandomSudokoBoard n

fillEmptyBoard :: Int -> SudokuBoard -> IO SudokuBoard    
fillEmptyBoard n sudokuBoard
    | n == 0 = return sudokuBoard
    | otherwise = do
        colNo <- generateRandomNumBetween '0' '8'
        rowNo <- generateRandomNumBetween '0' '8'
        val <- generateRandomNumBetween '1' '9'
        if isValidMove colNo rowNo val sudokuBoard 
            then fillEmptyBoard (n-1) (updateBoard colNo rowNo val sudokuBoard)
        else fillEmptyBoard n sudokuBoard

updateBoardBlocks :: SudokuBoard -> [(Int, Int, Int)] -> SudokuBoard        
updateBoardBlocks sudokuBoard coords =
    map (\row -> updateRow row coords) sudokuBoard
        where
            updateRow row coords = map (\cell -> updateCell cell coords) row
            updateCell (Cell colNo rowNo blockNo val) coords
                | (colNo, rowNo) `elem` [ (colNo, rowNo) | (colNo, rowNo, blockNo) <- coords] = Cell colNo rowNo (head $ [ blockNo | (colNo', rowNo', blockNo) <- coords, colNo == colNo' && rowNo == rowNo']) val
                | otherwise = Cell colNo rowNo blockNo val
-}

generateRandomSudokoBoard :: Int -> IO SudokuBoard
generateRandomSudokoBoard numOfEmptyCells = do
    file <- readFile "../maps/map2ans.txt"
    let sudokuBoard = fileToBoardConverter file
    sudokuBoard' <- generateRandomCompletedSudokoBoard sudokuBoard
    generateSudokoBoardWithCellsRemoved sudokuBoard' numOfEmptyCells

generateRandomCompletedSudokoBoard :: SudokuBoard -> IO SudokuBoard
generateRandomCompletedSudokoBoard sudokuBoard = do 
    randomList <- generateRandomList 9 '1' '9' True
    return $ updateBoardByRandomList sudokuBoard randomList

updateBoardByRandomList :: SudokuBoard -> [Int] -> SudokuBoard
updateBoardByRandomList sudokuBoard randomList = 
    map (\row -> updateRow row) sudokuBoard
    where 
        updateRow row = map (\cell -> updateCell cell) row
        updateCell (Cell colNo rowNo blockNo value) = Cell colNo rowNo blockNo (randomList !! (value-1))
    
generateSudokoBoardWithCellsRemoved :: SudokuBoard -> Int -> IO SudokuBoard
generateSudokoBoardWithCellsRemoved sudokuBoard n = do
    cellsToBeRemoved <- generateRandomCoords n
    return $ updateBoardByEmptyCells sudokuBoard cellsToBeRemoved

updateBoardByEmptyCells :: SudokuBoard -> [(Int, Int)] -> SudokuBoard
updateBoardByEmptyCells sudokuBoard cellsToBeRemoved = do
    map (\row -> updateRow row) sudokuBoard
    where 
        updateRow row = map (\cell -> updateCell cell) row
        updateCell (Cell colNo rowNo blockNo value)
            | (colNo, rowNo) `elem` cellsToBeRemoved = Cell colNo rowNo blockNo 0
            | otherwise = Cell colNo rowNo blockNo value
  
generateRandomCoords :: Int -> IO [(Int, Int)]
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




