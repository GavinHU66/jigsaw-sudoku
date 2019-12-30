
module Cell where

data Cell = Cell Int Int Int Int deriving (Eq)

type SudokuBoard = [[Cell]]

getColNo :: Cell -> Int
getColNo (Cell colNo _ _ _) = colNo

getRowNo :: Cell -> Int
getRowNo (Cell _ rowNo _ _) = rowNo

getBlockNo :: Cell -> Int
getBlockNo (Cell _ _ blockNo _) = blockNo

getVal :: Cell -> Int
getVal (Cell _ _ _ val) = val