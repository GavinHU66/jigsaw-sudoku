
module Cell where

data Cell = Cell Int Int Int Int deriving (Eq)

type SudokuBoard = [[Cell]]

-- Description: 
    -- Get the column index of a given cell.
-- Input:
    -- A Cell object
-- Output: 
    -- The column index of a given cell.
getColNo :: Cell -> Int
getColNo (Cell colNo _ _ _) = colNo

-- Description: 
    -- Get the row index of a given cell.
-- Input:
    -- A Cell object
-- Output: 
    -- The row index of a given cell.
getRowNo :: Cell -> Int
getRowNo (Cell _ rowNo _ _) = rowNo

-- Description: 
    -- Get the block index of a given cell.
-- Input:
    -- A Cell object
-- Output: 
    -- The block index of a given cell.
getBlockNo :: Cell -> Int
getBlockNo (Cell _ _ blockNo _) = blockNo

-- Description: 
    -- Get the value of a given cell.
-- Input:
    -- A Cell object
-- Output: 
    -- The value of a given cell.
getVal :: Cell -> Int
getVal (Cell _ _ _ val) = val