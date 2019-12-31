module Solver where

import Cell
import Utils
import RenderBoard
import Data.List
import Data.Char

type TryRecs = [(Int, Int, Int, [Int])]


-- Description: 
    -- Get a list of possible values that can fill up a given blank cell on a sudoku board
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- A list Int values representing the possible values that can
    -- be filled into a blank cell on a sudoku board
getPossVals :: SudokuBoard -> Cell -> [Int]
getPossVals board (Cell colNo rowNo blockNo value) =
    possVals
    where 
        rowVals = getRowVals board rowNo
        colVals = getColVals board colNo
        blockVals = getBlockVals board blockNo
        possVals = reduceFromList (rowVals++colVals++blockVals)


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
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- Return True if the sudoko board is full, False otherwise.
isFull :: SudokuBoard -> Bool
isFull = (== 0) . getnumZeroCells


-- Description: 
    -- Get the first blank cell on the given sudoko board.
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- A Cell object which is the first blank cell on the sudoko board.
getFirstBlank :: SudokuBoard -> Cell
getFirstBlank = 
    head . getAllZeroCells
    

-- Description: 
    -- Get the all blank cells on the given sudoko board.
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- A list of Cell objects which are blank on the sudoku board.
getAllZeroCells :: SudokuBoard -> [Cell]
getAllZeroCells board = 
    [ cell | cell <- concat board, getVal cell == 0 ]


-- Description: 
    -- Get the all coordinates of blank cells on the given sudoko board.
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- A list of coordinates of blank cells on the sudoku board.
getAllBlankCoords :: SudokuBoard -> [(Int, Int, Int)]
getAllBlankCoords board = 
    [ (getColNo cell, getRowNo cell, getBlockNo cell) | cell <- concat board, getVal cell == 0 ]



-- Description: 
    -- Get the number of blank cells on the given sudoko board.
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- The number of blank cells on the given sudoko board.
getnumZeroCells :: SudokuBoard -> Int
getnumZeroCells = 
    length . getAllZeroCells



-- Description: 
    -- Get a blank cell on the given sudoko board.
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- A IO Cell object which is a random blank cell on the sudoko board.
getBlankCell :: SudokuBoard -> IO Cell
getBlankCell board = do
    idx <- getRandNum 0 (length allZeroCells - 1)
    return $ allZeroCells !! idx
    where
        allZeroCells = getAllZeroCells board


-- Description: 
    -- Solve the sudoku board game.
    -- It uses DFS, for each empty cell, the algorithm only
    -- tries the poissble number instead of 1 ~ 9
-- Input:
    -- board: the SudokuBoard object to be converted
-- Output: 
    -- A (Bool, SudokuBoard) tuple
    -- first Bool value is true if the sudoku board game, False otherwise
    -- latter SudokuBoard is a SudokuBoard object solution if there is, a stucking point state of a sudoku board if not.
-- mySolver :: SudokuBoard -> (Bool, SudokuBoard)
mySolver board = 
    mySolver' board [] True


-- Description: 
--     Help to solve a 
-- Input:
--     board: the SudokuBoard object to be converted
--     tryRecs: a list of tried records
--     isNewPoint: True if the cell that we are investigating is a new cell, False otherwise
-- Output: 
--     A (Bool, SudokuBoard) tuple
--     first Bool value is true if the sudoku board game, False otherwise
--     latter SudokuBoard is a SudokuBoard object solution if there is, a stucking point state of a sudoku board if not.
mySolver' :: SudokuBoard -> TryRecs -> Bool -> (Bool, SudokuBoard)
mySolver' board tryRecs isNewPoint
    | isFull board = (True, board)
    | otherwise = do
        let allZeroCells = getAllZeroCells board
            numZeroCells = length allZeroCells
        if isNewPoint then do
            let cell = allZeroCells !! (numZeroCells `div` 2)
                possVals = getPossVals board cell
            if length possVals == 0 then do
                if length tryRecs == 0 then (False, board)
                else do
                    let (prevColNo, prevRowNo, triedNum, possVals) = (last tryRecs)
                        prevSudokuBoard = updateBoard prevColNo prevRowNo 0 board
                    mySolver' prevSudokuBoard tryRecs False
            else do 
                let newVal = (last possVals) -- no exception
                    colNo = getColNo cell
                    rowNo = getRowNo cell
                    newBoard = updateBoard colNo rowNo newVal board
                mySolver' newBoard (tryRecs++[(colNo, rowNo, newVal, possVals)]) True
        else do -- not a new cell, then we need to remove the failed possible value
            if length tryRecs == 0 then (False, board)
            else do
                let tryRecs' = removePossVal tryRecs
                    (colNo, rowNo, triedNum, possVals) = (last tryRecs')
                    (prevColNo, prevRowNo, prevTriedNum, prevPossVals) = (last (init tryRecs'))
                if length possVals == 0 then do
                    if length tryRecs' == 1 then (False, board)
                    else do 
                        let sudokuBoardSetCurrentCellToZero = updateBoard colNo rowNo 0 board
                            prevSudokuBoard = updateBoard prevColNo prevRowNo 0 sudokuBoardSetCurrentCellToZero
                        mySolver' prevSudokuBoard (init tryRecs') False
                else do 
                    let newBoard = updateBoard colNo rowNo newVal board
                        newVal = (last possVals) -- no exception
                    mySolver' newBoard ((init tryRecs')++[(colNo, rowNo, newVal, possVals)]) True

-- Description: 
    -- Update the move history with a possible value (which being tested to be impossible to be a value in the cell)
-- Input:
    -- tryRecs: a list of tried records
-- Output: 
    -- a list of tried records, with last one updated
removePossVal :: TryRecs -> TryRecs
removePossVal tryRecs = 
    (init tryRecs) ++ [(colNo, rowNo, 0, possVals')]
    where 
        (colNo, rowNo, triedNum, possVals) = last tryRecs
        possVals' = [ val | val <- possVals, val /= triedNum]


