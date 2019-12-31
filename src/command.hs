{- |
Module      :  Command
Description :  This module defined actions of commands using in the sudoku game.
Copyright   :  (c) Hu Qiyun Gavin 2019

Maintainer  :  gavin66@connect.hku.hk
-}

module Command where

import Data.Char
import Cell
import Utils
import RenderBoard
import Solver
import Generator

type MoveRecords = [(Int, Int, Int)]

{-|
    Description: 
        Parse and execute the user command.
    Input:
        cmd: the file content to be written
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution,
        Bool value is True if the end of the game is reached, False if not.
-}
exeCmd :: String -> SudokuBoard -> IO (SudokuBoard, Bool)
exeCmd cmd board
    | cmd == "move"  = exeCmdMove board [] [] "Please press the ENTER key, then indicate the location and value."
    | cmd == "save"  = exeCmdSave board
    | cmd == "load"  = do exeCmdLoad; return (board, False)
    | cmd == "quit"  = exeCmdQuit board
    | cmd == "new"   = exeCmdNew
    | cmd == "solve" = exeCmdSolve board
    | cmd == "help"  = do exeCmdHelp; return (board, False)

{-|
    Description: 
        Execute the user "load" command.
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution,
        Bool value is True if the end of the game is reached, False if not.   
-} 
exeCmdLoad :: IO (SudokuBoard, Bool)
exeCmdLoad = do  
    file <- safeRF
    let givenFile = lines file
        newBlockInfo = take (9) givenFile
        initialCellInfo = drop (9) givenFile
        newBoard = constructBoard newBlockInfo initialCellInfo
    printBoard newBoard
    return (newBoard, False)

{-|
    Description: 
        Execute the user "load" command.
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.
-}
exeCmdMove :: SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
exeCmdMove board undos redos prompt = do
    putStrLn ""
    putStrLn prompt
    putStrLn "To view all commands available, enter \"help\""
    preMoveCmd <- getLine
    exeCmdInMoveLoop preMoveCmd board undos redos prompt

exeCmdInMoveLoop :: String -> SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
exeCmdInMoveLoop preMoveCmd board undos redos prompt
    | preMoveCmd == "stop"  = return (board, False)
    | preMoveCmd == "hint"  = do exeCmdHint board; exeCmdMove board undos redos "Please indicate the location and value."
    | preMoveCmd == "board" = do printBoard board; exeCmdMove board undos redos "Please indicate the location and value."
    | preMoveCmd == "undo"  = exeCmdUndo board undos redos "Please indicate the location and value."
    | preMoveCmd == "redo"  = exeCmdRedo board undos redos "Please indicate the location and value."
    | preMoveCmd == "help"  = do 
            putStrLn "\n               Command   Usage\n" 
            putStrLn "               \"stop\"  - To stop the move, jump out of the loop of filling.\n" 
            putStrLn "               \"hint\"  - It prompt you with the of possible values could be"
            putStrLn "                         filled into the next blank cell.\n" 
            putStrLn "               \"board\" - To print the board to the console.\n" 
            putStrLn "               \"undo\"  - To undo the fill-cell action if there exists more" 
            putStrLn "                         than zero previous actions.\n"
            putStrLn "               \"redo\"  - To redo the fill-cell action if there exists more" 
            putStrLn "                         than zero undo-ed actions.\n"
            putStrLn "               \"help\"  - Display the commands and their usage" 
            exeCmdMove board undos redos prompt
    | preMoveCmd == ""      = do
        rowNo <- askRow "Select a row (0-8):"
        colNo <- askCol "Select a column (0-8):"
        value <- askValue "Type in a value (1-9):"
        if (isValidMove colNo rowNo value board) then do 
            let newBoard = updateBoard colNo rowNo value board
            putStrLn "\nNew board:" 
            printBoard newBoard 
            if isGameOver newBoard then do 
                putStrLn "Congratulations, you finished the game!"
                return (newBoard, False)
            else exeCmdMove newBoard (undos++[(colNo, rowNo, value)]) redos "Please indicate the location and value."
        else if not $ isBlank board colNo rowNo
            then exeCmdMove board undos redos "Conflict! There point is already been filled! Choose another point!"
        else if (not $ isRowValid board rowNo value) && (isColValid board colNo value)
            then exeCmdMove board undos redos "Conflict! This number has already existed in this row! Please try again!"
        else if (isRowValid board rowNo value) && (not $ isColValid board colNo value)
            then exeCmdMove board undos redos "Conflict! This number has already existed in this column! Please try again!"
        else if (not $ isRowValid board rowNo value) && (not $ isColValid board colNo value)
            then exeCmdMove board undos redos "Conflict! This number has already existed in both row and column! Please try again!"
        else if (not $ isBlockValid board colNo rowNo value)
            then exeCmdMove board undos redos "Conflict! This number has already existed in this block! Please try again!"
        else exeCmdMove board undos redos "Sorry, there is a conflict!"
    | otherwise = exeCmdMove board undos redos "Command not correct, try again!"

{-|
    Description: 
        Execute the user "save" command.
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution, same with the input,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.  Should be False in this case.
-}
exeCmdSave :: SudokuBoard -> IO (SudokuBoard, Bool)
exeCmdSave board = do  
    saveToFile board
    return (board, False)

{-|
    Description: 
        Execute the user "quit" command.
        It prompts the user with an opportunity to confirm a quit action,
        once being confirmed, it prompts the user with an opportunity to save the current game.
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution, same with the input,
        Bool value is True if the user finally confirmed, False if not.
-}
exeCmdQuit :: SudokuBoard -> IO (SudokuBoard, Bool)
exeCmdQuit board = do
    putStrLn "Are you sure to quit?(y/n)"
    willQuit <- getLine
    if willQuit == "n" then return (board, False)
    else if willQuit == "y" then confirmSaveGame board
    else exeCmdQuit board    

{-|
    Description: 
        It prompts the user with an opportunity to confirm a save action
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution, same with the input,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.   
-}
confirmSaveGame :: SudokuBoard -> IO (SudokuBoard, Bool)
confirmSaveGame board = do 
    putStrLn "Do you want to save?(y/n)"
    willSave <- getLine 
    if willSave == "n" then do putStrLn "Quited without saving!"; return (board, True)
    else if willSave == "y" then do saveToFile board; putStrLn "Quited with the game being saved successfully"; return (board, True)
    else confirmSaveGame board

{-|
    Description: 
        Execute the user "redo" command.
        It un-dos the fill-cell action if there exists more than zero previous actions.
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case. 
-}
exeCmdUndo :: SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
exeCmdUndo board undos redos prompt
    | undos == [] = do putStrLn "There is no action to be undo!"; exeCmdMove board undos redos prompt
    | otherwise = do
        let (colNo, rowNo, value) = last undos
            newBoard = updateBoard colNo rowNo 0 board
        putStrLn "\nNew board:" 
        printBoard newBoard 
        exeCmdMove newBoard (init undos) (redos++[last undos]) prompt

{-|
    Description: 
        Execute the user "redo" command.
        It re-dos the fill-cell action if there exists more than zero undo-ed actions.
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.
-}
exeCmdRedo :: SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
exeCmdRedo board undos redos prompt
    | redos == [] = do putStrLn "There is no action to be redo"; exeCmdMove board undos redos prompt
    | otherwise = do
        let (colNo, rowNo, value) = last redos
            newBoard = updateBoard colNo rowNo value board
        putStrLn "\nNew board:" 
        printBoard newBoard 
        exeCmdMove newBoard (undos++[last redos]) (init redos) prompt 

{-|
    Description: 
        Execute the user "solve" command.
        It prompt the user with a solution of the current sudoku board, without ending up the game.
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution, same with the input,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.   
-}
exeCmdSolve :: SudokuBoard -> IO (SudokuBoard, Bool)
exeCmdSolve board = do
    putStrLn "Generating a possible answer, it may take longer time if there is 50+ blank cells, please wait..."
    let (hasAnswer, board') = mySolver board
    do  if hasAnswer then printBoard board' else putStrLn "The current board is unsolveable! Please retry"
    return (board, False)

{-|
    Description: 
        Execute the user "hint" command.
        It prompt the user with the of possible values could be filled into the next blank cell 
    Input:
        board: a SudokuBoard object representing the current sudoku board the user is playing with
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution, same with the input,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.
-}
exeCmdHint :: SudokuBoard -> IO (SudokuBoard, Bool)
exeCmdHint board = do
    putStrLn ("You can try to fill the cell at column-" ++ [intToDigit $ getColNo $ getFirstBlank $ board] ++ " row-" ++ [intToDigit $ getRowNo $ getFirstBlank $ board] ++ " with the possible number " ++ possibleValsStr)
    return (board, False)
    where 
        possibleValsStr = map (intToDigit) [ value | value <- getPossVals board $ getFirstBlank $ board ]

{-|
    Description: 
        Execute the user "new" command.
        Generate a new random sudoku board to user
    Output: 
        a IO (SudokuBoard, Bool) object, 
        SudokuBoard is the SudokuBoard object after the execution,
        Bool value is True if the end of the game is reached, False if not. Should be False in this case.
-}
exeCmdNew :: IO (SudokuBoard, Bool)
exeCmdNew = do
    numOfEmptyCells <- askLevel
    newBoard <- randBoard numOfEmptyCells
    printBoard newBoard
    return (newBoard, False)
    where 
        askLevel = do
            putStrLn "Choose level(easy/medium/hard):"
            level <- getLine
            if level == "easy" 
                then return 30
            else if level == "medium" 
                then return 40
            else if level == "hard" 
                then return 50 
            else askLevel
{-|
    Description: 
        Execute the user "help" command.
        It prompts the user with the usage of all commands.
    Output: 
        a IO () object
-}
exeCmdHelp :: IO ()
exeCmdHelp = do
    putStrLn "\n               Command   Usage\n" 
    putStrLn "               \"move\"  - To fill up a point, once in the loop of move, you" 
    putStrLn "                         would be offered more functionalities, such as redo," 
    putStrLn "                         undo, hint, etc.\n" 
    putStrLn "               \"save\"  - To save the current board of the sudoko game to an" 
    putStrLn "                         indicated file.\n" 
    putStrLn "               \"load\"  - To load a board of sudoko game from an indicated file.\n" 
    putStrLn "               \"quit\"  - To quit the current sudoko game after your confirmation." 
    putStrLn "                         You would also be offered a chance to save the current"
    putStrLn "                         board of the sudoko game to an indicated file.\n" 
    putStrLn "               \"new\"   - Generate a new random sudoku board, you can chooes the" 
    putStrLn "                         level of difficulty.\n" 
    putStrLn "               \"solve\" - Show a solution of the current sudoku game, without ending" 
    putStrLn "                         up the game, you could still work on your game.\n" 
