module Command where

import Data.Char
import Cell
import Utils
import RenderBoard
import Solver
import Generator

type MoveRecords = [(Int, Int, Int)]

-- Description: 
    -- Parse and execute the user command.
-- Input:
    -- cmd: the file content to be written
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution,
    -- Bool value is True if the end of the game is reached, False if not.
executeCmd :: String -> SudokuBoard -> IO (SudokuBoard, Bool)
executeCmd cmd sudokuBoard
    | cmd == "move"  = executeCmdMove sudokuBoard [] [] "Please indicate the location and value."
    | cmd == "save"  = executeCmdSave sudokuBoard
    | cmd == "load"  = executeCmdLoad sudokuBoard  
    | cmd == "quit"  = executeCmdQuit sudokuBoard
    | cmd == "new"   = executeCmdNew sudokuBoard
    | cmd == "solve" = executeCmdSolve sudokuBoard

-- Description: 
    -- Execute the user "load" command.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution,
    -- Bool value is True if the end of the game is reached, False if not.    
executeCmdLoad :: SudokuBoard -> IO (SudokuBoard, Bool)
executeCmdLoad sudokuBoard = do  
    file <- safeRF
    let givenFile = lines file
        newBlockInfo = take (9) givenFile
        initialCellInfo = drop (9) givenFile
        newSudokuBoard = constructBoard newBlockInfo initialCellInfo
    printBoard newSudokuBoard
    return (newSudokuBoard, False)

-- Description: 
    -- Execute the user "load" command.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.
executeCmdMove :: SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo prompt = do
    putStrLn ""
    putStrLn prompt
    putStrLn "Available command \"stop\", \"undo\", \"redo\", \"hint\", \"board\", or press enter key to move "
    preMoveCmd <- getLine
    if preMoveCmd == "stop" 
        then return (sudokuBoard, False)
    else if preMoveCmd == "hint" 
        then do executeCmdHint sudokuBoard
                executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "board" 
        then do printBoard sudokuBoard
                executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "undo" 
        then do executeCmdUndo sudokuBoard movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "redo" 
        then do executeCmdRedo sudokuBoard movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "" 
        then do
            rowNo <- askRow "Select a row (0-8):"
            colNo <- askCol "Select a column (0-8):"
            value <- askValue "Type in a value (1-9):"
            if (isValidMove colNo rowNo value sudokuBoard) 
                then do 
                    let newSudokuBoard = updateBoard colNo rowNo value sudokuBoard
                    putStrLn "\nNew board:" 
                    printBoard newSudokuBoard 
                    if isGameOver newSudokuBoard
                        then do 
                            putStrLn "Congratulations, you finished the game!"
                            return (newSudokuBoard, False)
                    else executeCmdMove newSudokuBoard (movesToBeUndo++[(colNo, rowNo, value)]) movesToBeRedo "Please indicate the location and value."
            else if not $ isBlank sudokuBoard colNo rowNo
                then executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Conflict! There point is already been filled! Choose another point!"
            else if (not $ isRowValid sudokuBoard rowNo value) && (isColValid sudokuBoard colNo value)
                then executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Conflict! This number has already existed in this row! Please try again!"
            else if (isRowValid sudokuBoard rowNo value) && (not $ isColValid sudokuBoard colNo value)
                then executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Conflict! This number has already existed in this column! Please try again!"
            else if (not $ isRowValid sudokuBoard rowNo value) && (not $ isColValid sudokuBoard colNo value)
                then executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Conflict! This number has already existed in both row and column! Please try again!"
            else if (not $ isBlockValid sudokuBoard colNo rowNo value)
                then executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Conflict! This number has already existed in this block! Please try again!"
            else executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Sorry, there is a conflict!"
    else executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo "Command not correct, try again!"

-- Description: 
    -- Execute the user "save" command.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution, same with the input,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.  Should be False in this case.
executeCmdSave :: SudokuBoard -> IO (SudokuBoard, Bool)
executeCmdSave sudokuBoard = do  
    saveToFile sudokuBoard
    return (sudokuBoard, False)

-- Description: 
    -- Execute the user "quit" command.
    -- It prompts the user with an opportunity to confirm this quit action,
    -- once being confirmed, it prompts the user with an opportunity to save the current game.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution, same with the input,
    -- Bool value is True if the user finally confirmed, False if not.
executeCmdQuit :: SudokuBoard -> IO (SudokuBoard, Bool)
executeCmdQuit sudokuBoard = do
    putStrLn "Are you sure to quit?(y/n)"
    willQuit <- getLine
    if willQuit == "n"
        then return (sudokuBoard, False)
    else if willQuit == "y"
        then confirmSaveGame sudokuBoard
    else executeCmdQuit sudokuBoard    

-- Description: 
    -- Execute the user "save" command.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution, same with the input,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.   
confirmSaveGame :: SudokuBoard -> IO (SudokuBoard, Bool)
confirmSaveGame sudokuBoard = do 
    putStrLn "Do you want to save?(y/n)"
    willSave <- getLine 
    if willSave == "n"
        then do 
            putStrLn "Quited without saving!"
            return (sudokuBoard, True)
    else if willSave == "y"
        then do saveToFile sudokuBoard
                putStrLn "Quited with the game being saved successfully"
                return (sudokuBoard, True)
    else confirmSaveGame sudokuBoard

-- Description: 
    -- Execute the user "redo" command.
    -- It un-dos the fill-cell action if there exists more than one previous actions.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case. 
executeCmdUndo :: SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
executeCmdUndo sudokuBoard movesToBeUndo movesToBeRedo prompt = do
    if length movesToBeUndo == 0 
        then do 
            putStrLn "There is no action to be undo"
            executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo prompt
    else do
        let (colNo, rowNo, value) = last movesToBeUndo
            newSudokuBoard = updateBoard colNo rowNo 0 sudokuBoard
        putStrLn "\nNew board:" 
        printBoard newSudokuBoard 
        executeCmdMove newSudokuBoard (init movesToBeUndo) (movesToBeRedo++[last movesToBeUndo]) prompt

-- Description: 
    -- Execute the user "redo" command.
    -- It re-dos the fill-cell action if there exists more than one undo-ed actions.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.
executeCmdRedo :: SudokuBoard -> MoveRecords -> MoveRecords -> String -> IO (SudokuBoard, Bool)
executeCmdRedo sudokuBoard movesToBeUndo movesToBeRedo prompt = do
    if length movesToBeRedo == 0 
        then do 
            putStrLn "There is no action to be redo"
            executeCmdMove sudokuBoard movesToBeUndo movesToBeRedo prompt
    else do
        let (colNo, rowNo, value) = last movesToBeRedo
            newSudokuBoard = updateBoard colNo rowNo value sudokuBoard
        putStrLn "\nNew board:" 
        printBoard newSudokuBoard 
        executeCmdMove newSudokuBoard (movesToBeUndo++[last movesToBeRedo]) (init movesToBeRedo) prompt 

-- Description: 
    -- Execute the user "solve" command.
    -- It prompt the user with a solution of the current sudoku board, without ending up the game.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution, same with the input,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.   
executeCmdSolve :: SudokuBoard -> IO (SudokuBoard, Bool)
executeCmdSolve sudokuBoard = do
    putStrLn "Generating a possible answer, it may take longer time if there is 50+ blank cells, please wait..."
    let (hasAnswer, board) = mySolver sudokuBoard
    do  if hasAnswer then printBoard board else putStrLn "The current board is unsolveable! Please retry"
    return (sudokuBoard, False)

-- Description: 
    -- Execute the user "hint" command.
    -- It prompt the user with the of possible values could be filled into the next blank cell 
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution, same with the input,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.
executeCmdHint :: SudokuBoard -> IO (SudokuBoard, Bool)
executeCmdHint sudokuBoard = do
    putStrLn ("You can try to fill the cell at column-" ++ [intToDigit $ getColNo nextCell] ++ " row-" ++ [intToDigit $ getRowNo nextCell] ++ " with the possible number " ++ possibleValsStr)
    return (sudokuBoard, False)
    where 
        possibleValsStr = [ intToDigit value | value <- possibleVals ]
        possibleVals = getPossibleVals sudokuBoard nextCell
        nextCell = getFirstBlank sudokuBoard

-- Description: 
    -- Execute the user "new" command.
    -- Generate a new random sudoku borad to user
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution,
    -- Bool value is True if the end of the game is reached, False if not. Should be False in this case.
executeCmdNew :: SudokuBoard -> IO (SudokuBoard, Bool)
executeCmdNew sudokuBoard = do
    numOfEmptyCells <- askLevel
    newSudokuBoard <- generateRandomSudokoBoard numOfEmptyCells
    printBoard newSudokuBoard
    return (newSudokuBoard, False)
    where 
        askLevel = do
            putStrLn "Choose level(easy/medium/hard):"
            level <- getLine
            if level == "easy" 
                then return 30
            else if level == "medium" 
                then return 45
            else if level == "hard" 
                then return 60 
            else askLevel

-- Description: 
    -- Execute the user "load" command.
-- Input:
    -- sudokuBoard: a SudokuBoard object representing the current sudoku board the user is playing with
-- Output: 
    -- a IO (SudokuBoard, Bool) object, 
    -- SudokuBoard is the SudokuBoard object after the execution, same with the input,
    -- Bool value is True if the end of the game is reached, False if not.  Should be False in this case.  
executeCmdHelp = undefined 
