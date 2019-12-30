module Solver where

import Cell
import Utils
import RenderBoard

type TryHistory = [(Int, Int, Int, [Int])]

-- Description: 
    -- Get a list of possible values that can fill up a given blank cell on a sudoku board
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- A list Int values representing the possible values that can
    -- be filled into a blank cell on a sudoku board
getPossibleVals :: SudokuBoard -> Cell -> [Int]
getPossibleVals sudokuBoard (Cell colNo rowNo blockNo value) =
    possibleVals
    where 
        rowVals = getRowVals sudokuBoard rowNo
        colVals = getColVals sudokuBoard colNo
        blockVals = getBlockVals sudokuBoard blockNo
        possibleVals = reduceFromList (rowVals++colVals++blockVals)

-- Description: 
    -- Eliminate the values in a list from another list 
-- Input:
    -- existedVals: the list to be eliminated
-- Output: 
    -- A list after the elimination
reduceFromList :: [Int] -> [Int]
reduceFromList existedVals = 
    [ val | val <- [1..9], not $ val `elem` existedVals ]

-- Description: 
    -- Check if a sudoko board is full.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- Return True if the sudoko board is full, False otherwise.
isFull :: SudokuBoard -> Bool
isFull sudokuBoard
    | (getNumOfBlankCells sudokuBoard ) /= 0 = False
    | otherwise = isGameOver sudokuBoard   

-- Description: 
    -- Get the first blank cell on the given sudoko board.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- A Cell object which is the first blank cell on the sudoko board.
getFirstBlank :: SudokuBoard -> Cell
getFirstBlank sudokuBoard = 
    head [ (sudokuBoard !! rowNo) !! colNo | rowNo <- [0..8], colNo <- [0..8], isBlank sudokuBoard colNo rowNo ]
    
-- Description: 
    -- Get the all blank cells on the given sudoko board.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- A list of Cell objects which are blank on the sudoku board.
getAllBlankCells :: SudokuBoard -> [Cell]
getAllBlankCells sudokuBoard = 
    [ cell | cell <- concat sudokuBoard, getVal cell == 0 ]

-- Description: 
    -- Get the all coordinates of blank cells on the given sudoko board.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- A list of coordinates of blank cells on the sudoku board.
getAllBlankCoords :: SudokuBoard -> [(Int, Int)]
getAllBlankCoords sudokuBoard = 
    [ (getColNo cell, getRowNo cell) | cell <- concat sudokuBoard, getVal cell == 0 ]

-- Description: 
    -- Get the number of blank cells on the given sudoko board.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- The number of blank cells on the given sudoko board.
getNumOfBlankCells :: SudokuBoard -> Int
getNumOfBlankCells = 
    length . getAllBlankCells

-- Description: 
    -- Get a blank cell on the given sudoko board.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- A Cell object which is a blank cell on the sudoko board.
getBlankCell :: SudokuBoard -> Cell
getBlankCell sudokuBoard = 
    allBlankCells !! middleIdx
    where 
        allBlankCells = getAllBlankCells sudokuBoard
        middleIdx = (length allBlankCells) `div` 2

-- Description: 
    -- Solve the sudoku board game.
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
-- Output: 
    -- A (Bool, SudokuBoard) tuple
    -- first Bool value is true if the sudoku board game, False otherwise
    -- latter SudokuBoard is a SudokuBoard object solution if there is, a stucking point state of a sudoku board if not.
mySolver :: SudokuBoard -> (Bool, SudokuBoard)
mySolver sudokuBoard = 
    mySolver' sudokuBoard [] True

-- Description: 
    -- Convert a file to a SudokuBoard object
-- Input:
    -- sudokuBoard: the SudokuBoard object to be converted
    -- tryHistory: a list of tried records
    -- isNewPoint: True if the cell that we are investigating is a new cell, False otherwise
-- Output: 
    -- A (Bool, SudokuBoard) tuple
    -- first Bool value is true if the sudoku board game, False otherwise
    -- latter SudokuBoard is a SudokuBoard object solution if there is, a stucking point state of a sudoku board if not.
mySolver' :: SudokuBoard -> TryHistory -> Bool -> (Bool, SudokuBoard)
mySolver' sudokuBoard tryHistory isNewPoint = do
    let allBlankCells = getAllBlankCells sudokuBoard
        numOfBlankCells = length allBlankCells
    if isFull sudokuBoard then (True, sudokuBoard)
    else if numOfBlankCells == 1 && length (getPossibleVals sudokuBoard (allBlankCells !! 0)) == 0 then (False, sudokuBoard)
    else do 
        if isNewPoint 
            then do
                let cell = allBlankCells !! (numOfBlankCells `div` 2)
                    possibleVals = getPossibleVals sudokuBoard cell
                if length possibleVals == 0
                    then do
                        if length tryHistory == 0 then (False, sudokuBoard)
                        else do
                            let (prevColNo, prevRowNo, lastTriedNumber, possibleVals) = (last tryHistory)
                                prevSudokuBoard = updateBoard prevColNo prevRowNo 0 sudokuBoard
                            mySolver' prevSudokuBoard tryHistory False
                else do 
                    let newValue = (last possibleVals) -- no exception
                        colNo = getColNo cell
                        rowNo = getRowNo cell
                        newSudokuBoard = updateBoard colNo rowNo newValue sudokuBoard
                    mySolver' newSudokuBoard (tryHistory++[(colNo, rowNo, newValue, possibleVals)]) True
        else do -- not a new cell, then we need to remove the failed possible value
            if length tryHistory == 0 
                then (False, sudokuBoard)
            else do
                let tryHistory' = removePossibleValue tryHistory
                    (colNo, rowNo, lastTriedNumber, possibleVals) = (last tryHistory')
                    (prevColNo, prevRowNo, prevLastTriedNumber, prevPossibleVals) = (last (init tryHistory'))
                if length possibleVals == 0
                    then do
                        if length tryHistory' == 1 then (False, sudokuBoard)
                        else do 
                            let sudokuBoardSetCurrentCellToZero = updateBoard colNo rowNo 0 sudokuBoard
                                prevSudokuBoard = updateBoard prevColNo prevRowNo 0 sudokuBoardSetCurrentCellToZero
                            mySolver' prevSudokuBoard (init tryHistory') False
                else do 
                    let newSudokuBoard = updateBoard colNo rowNo newValue sudokuBoard
                        newValue = (last possibleVals) -- no exception
                    mySolver' newSudokuBoard ((init tryHistory')++[(colNo, rowNo, newValue, possibleVals)]) True

-- Description: 
    -- Update the move history with a possible value (which being tested to be impossible to be a value in the cell)
-- Input:
    -- tryHistory: a list of tried records
-- Output: 
    -- a list of tried records, with last one updated
removePossibleValue :: TryHistory -> TryHistory
removePossibleValue tryHistory = 
    (init tryHistory) ++ [(colNo, rowNo, 0, possibleVals')]
    where 
        (colNo, rowNo, lastTriedNumber, possibleVals) = last tryHistory
        possibleVals' = [ val | val <- possibleVals, val /= lastTriedNumber]

-- Description: 
    -- Convert a file to a SudokuBoard object
-- Input:
    -- file: the file to be converted
-- Output: 
    -- a SudokuBoard object converted from the given file
mySolverTester :: SudokuBoard -> TryHistory -> Bool -> IO ()
mySolverTester sudokuBoard tryHistory isNewPoint = do
    putStrLn (show tryHistory)
    let allBlankCells = getAllBlankCells sudokuBoard
        numOfBlankCells = length allBlankCells
    if isFull sudokuBoard
        then putStrLn "1"
    else if numOfBlankCells == 1 && length (getPossibleVals sudokuBoard (allBlankCells !! 0)) == 0
        then putStrLn "2"
    else do 
        if isNewPoint 
            then do
                putStrLn "4"
                let cell = allBlankCells !! (numOfBlankCells `div` 2)
                    possibleVals = getPossibleVals sudokuBoard cell
                putStrLn (show possibleVals)
                if length possibleVals == 0
                    then do
                        putStrLn "5"
                        if length tryHistory == 0 
                            then putStrLn "10" 
                        else do
                            putStrLn "11"
                            let (colNo, rowNo, lastTriedNumber, possibleVals) = (last tryHistory)
                                prevSudokuBoard = updateBoard colNo rowNo 0 sudokuBoard
                            mySolverTester prevSudokuBoard tryHistory False
                else do 
                    putStrLn "6"
                    let newValue = (last possibleVals) -- no exception
                        colNo = getColNo cell
                        rowNo = getRowNo cell
                        newSudokuBoard = updateBoard (getColNo cell) (getRowNo cell) newValue sudokuBoard
                    mySolverTester newSudokuBoard (tryHistory++[(colNo, rowNo, newValue, possibleVals)]) True
        else do -- not a new cell, then we need to remove the failed possible value
            if length tryHistory == 0 
                then putStrLn "3"
            else do
                putStrLn "7"
                let tryHistory' = removePossibleValue tryHistory
                    (colNo, rowNo, lastTriedNumber, possibleVals) = (last tryHistory')
                    (prevColNo, prevRowNo, prevLastTriedNumber, prevPossibleVals) = (last (init tryHistory'))
                if length possibleVals == 0
                    then do
                        if length tryHistory' == 1 
                            then putStrLn "12"
                        else do 
                            putStrLn "8"
                            let sudokuBoardSetCurrentCellToZero = updateBoard colNo rowNo 0 sudokuBoard
                                prevSudokuBoard = updateBoard prevColNo prevRowNo 0 sudokuBoardSetCurrentCellToZero
                            mySolverTester prevSudokuBoard (init tryHistory') False
                else do 
                    putStrLn "9"
                    let newSudokuBoard = updateBoard colNo rowNo newValue sudokuBoard
                        newValue = (last possibleVals) -- no exception
                    mySolverTester newSudokuBoard ((init tryHistory')++[(colNo, rowNo, newValue, possibleVals)]) True


-- mySolver2 :: SudokuBoard -> TryHistory -> Bool -> [Cell] -> (Bool, SudokuBoard)
-- mySolver2 sudokuBoard tryHistory isNewPoint allBlankCoords
--     | length allBlankCoords == 0 = (True, sudokuBoard)
--     | otherwise = do
--         if isNewPoint 
--             then do
--                 let (colNo, rowNo) = allBlankCoords !! 0
--                     possibleVals = getPossibleVals sudokuBoard ()
--                 if length possibleVals == 0
--                     then do
--                         if length tryHistory == 0 then (False, sudokuBoard)
--                         else do
--                             let (prevColNo, prevRowNo, lastTriedNumber, possibleVals) = (last tryHistory)
--                                 prevSudokuBoard = updateBoard prevColNo prevRowNo 0 sudokuBoard
--                             mySolver2 prevSudokuBoard tryHistory False (allBlankCells++[])
--                 else do 
--                     let newValue = (last possibleVals) -- no exception
--                         colNo = getColNo cell
--                         rowNo = getRowNo cell
--                         newSudokuBoard = updateBoard colNo rowNo newValue sudokuBoard
--                     mySolver2 newSudokuBoard (tryHistory++[(colNo, rowNo, newValue, possibleVals)]) True (tail allBlankCells)
--         else do -- not a new cell, then we need to remove the failed possible value
--             if length tryHistory == 0 
--                 then (False, sudokuBoard)
--             else do
--                 let tryHistory' = removePossibleValue tryHistory
--                     (colNo, rowNo, lastTriedNumber, possibleVals) = (last tryHistory')
--                     (prevColNo, prevRowNo, prevLastTriedNumber, prevPossibleVals) = (last (init tryHistory'))
--                 if length possibleVals == 0
--                     then do
--                         if length tryHistory' == 1 then (False, sudokuBoard)
--                         else do 
--                             let sudokuBoardSetCurrentCellToZero = updateBoard colNo rowNo 0 sudokuBoard
--                                 prevSudokuBoard = updateBoard prevColNo prevRowNo 0 sudokuBoardSetCurrentCellToZero
--                             mySolver' prevSudokuBoard (init tryHistory') False
--                 else do 
--                     let newSudokuBoard = updateBoard colNo rowNo newValue sudokuBoard
--                         newValue = (last possibleVals) -- no exception
--                     mySolver' newSudokuBoard ((init tryHistory')++[(colNo, rowNo, newValue, possibleVals)]) True