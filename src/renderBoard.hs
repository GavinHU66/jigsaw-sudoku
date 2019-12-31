{- |
Module      :  Generator
Description :  This module defined methods of printing a random sudoku board on the console.
Copyright   :  (c) Hu Qiyun Gavin 2019

Maintainer  :  gavin66@connect.hku.hk
-}

module RenderBoard where

import Data.Char
import Utils
import Cell
{-|
    Description: 
        Print a file to the console
    Input:
        file: the file representing the sudoku board
    Output: 
        an IO () object
-}
printFile :: String -> IO ()   
printFile = printBoard . fileToBoardConverter

{-|
    Description: 
        Print a sudoku board to the console
    Input:
        board: the SudokuBoard object representing the sudoku board
    Output: 
        an IO () object
-}
printBoard :: SudokuBoard -> IO ()
printBoard board = 
    putStrLn $ unlines $ printedBoard
        where 
            printedBoard = [""] ++ line1 ++ line2 ++ line3 ++ line4 ++ line5 ++ line6
            line1 = ["                    Column Index                                    "] 
            line2 = ["                        0    1    2    3    4    5    6    7    8   "]
            line3 = ["           Row Index  _____________________________________________ "] 
            line4 = ["                     |                                             |"] 
            line5 = init $ concat [ printRow board row idx | (row, idx) <- zip board [0..8] ]
            line6 = ["                     |_____________________________________________|"]

{-|
    Description: 
        Convert a row to a list of String pattern.
    Input:
        board: the SudokuBoard object representing the sudoku board
        boardRow: the row of a sudoku board
        rowNo: the row index
    Output: 
        A list of String pattern to be printed.
-}
printRow :: SudokuBoard -> [Cell] -> Int -> [String]
printRow board boardRow rowNo = 
    ["                  " ++ [intToDigit rowNo] ++ "  |" ++ concat (map (\cell -> printCell board cell) boardRow) ++ "|"]
    ++ ["                     |" ++ concat (map (\cell -> printHorizontalBlockLine board cell) boardRow) ++ "|"]

{-|
    Description: 
        Convert a cell to a String pattern.
    Input:
        board: the SudokuBoard object representing the sudoku board
        cell: the cell of a sudoku board
    Output: 
        A String pattern to be printed.
-}
printCell :: SudokuBoard -> Cell -> String
printCell board (Cell colNo rowNo blockNo value)
    | colNo < 8 && blockNo /= (getBlockNo $ getCell (colNo+1) rowNo board) = "  " ++ [printNumber value] ++ " |"
    | otherwise = "  " ++ [printNumber value] ++ "  " 
  
{-|
    Description: 
        Convert a value of a cell to a char pattern.
    Input:
        value: the value of a cell
    Output: 
        A char pattern to be printed.
-}
printNumber :: Int -> Char
printNumber value
    | value == 0 = '.'
    | otherwise  = intToDigit value

{-|
    Description: 
        Handle the horizontal line pattern.
    Input:
        board: the SudokuBoard object representing the sudoku board
        cell: the cell of a sudoku board
    Output: 
        A String prepresenting the horizontal line pattern.
-}
printHorizontalBlockLine :: SudokuBoard -> Cell -> String
printHorizontalBlockLine board (Cell colNo rowNo blockNo value)
    | rowNo < 8 && blockNo /= (getBlockNo $ getCell colNo (rowNo+1) board) = "-----"
    | rowNo < 8 && blockNo == (getBlockNo $ getCell colNo (rowNo+1) board) = "     "
    | rowNo == 8 = "     "


