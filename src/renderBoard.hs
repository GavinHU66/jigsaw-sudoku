
module RenderBoard where

import Data.Char
import Utils
import Cell
                
printFile :: String -> IO ()   
printFile = printBoard . fileToBoardConverter

printBoard :: SudokuBoard -> IO ()
printBoard sudokuBoard = 
    putStrLn $ unlines $ printedBoard
        where 
            printedBoard = [""] ++ line1 ++ line2 ++ line3 ++ line4 ++ line5 ++ line6
            line1 = ["                    Column Index                                    "] 
            line2 = ["                        0    1    2    3    4    5    6    7    8   "]
            line3 = ["           Row Index  _____________________________________________ "] 
            line4 = ["                     |                                             |"] 
            line5 = init $ concat [ printRow sudokuBoard row idx | (row, idx) <- zip sudokuBoard [0..8] ]
            line6 = ["                     |_____________________________________________|"]

printRow :: SudokuBoard -> [Cell] -> Int -> [String]
printRow sudokuBoard sudokuBoardRow rowNo = 
    ["                  " ++ [intToDigit rowNo] ++ "  |" ++ concat (map (\cell -> printCell cell sudokuBoard) sudokuBoardRow) ++ "|"]
    ++ ["                     |" ++ concat (map (\cell -> printHorizontalBlockLine cell sudokuBoard) sudokuBoardRow) ++ "|"]

printCell :: Cell -> SudokuBoard -> String
printCell (Cell colNo rowNo blockNo value) sudokuBoard
    | colNo < 8 && blockNo /= (getBlockNo $ getCell (colNo+1) rowNo sudokuBoard) = "  " ++ [printNumber value] ++ " |"
    | otherwise = "  " ++ [printNumber value] ++ "  " 
  
printNumber :: Int -> Char
printNumber value
    | value == 0 = '.'
    | otherwise  = intToDigit value

printHorizontalBlockLine :: Cell -> SudokuBoard -> String
printHorizontalBlockLine (Cell colNo rowNo blockNo value) sudokuBoard
    | rowNo < 8 && blockNo /= (getBlockNo $ getCell colNo (rowNo+1) sudokuBoard) = "-----"
    | rowNo < 8 && blockNo == (getBlockNo $ getCell colNo (rowNo+1) sudokuBoard) = "     "
    | rowNo == 8 = "     "


