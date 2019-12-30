module Command where

import Data.Char
import Cell
import Utils
import RenderBoard
import Solver
import Generator

type MoveRecords = [(Int, Int, Int)]

sudokuGameLoop :: SudokuBoard -> [String] -> IO ()
sudokuGameLoop sudokuBoard blockInfo =
    do  putStrLn ""
        cmd <- askCmd "please enter your command, enter help to view all commands, to get started, type in \"move\"!"
        executeCmd cmd sudokuBoard blockInfo

executeCmd :: String -> SudokuBoard -> [String] -> IO ()
executeCmd cmd sudokuBoard blockInfo
    | cmd == "move" = executeCmdMove sudokuBoard blockInfo [] [] "Please indicate the location and value."
    | cmd == "save" = executeCmdSave sudokuBoard blockInfo "Please indicate the file name or path to be saved:\ne.g., \"./YourPath/filename\" to save in the indicated directory, or \"(./)filename\" to save in the current directory"
    | cmd == "load" = executeCmdLoad sudokuBoard blockInfo "Please indicate the file name to be loaded:"  
    | cmd == "quit" = executeCmdQuit sudokuBoard blockInfo
    | cmd == "new" = executeCmdNew sudokuBoard blockInfo
    | cmd == "solve" = do  
        executeCmdSolver sudokuBoard blockInfo
        sudokuGameLoop sudokuBoard blockInfo 
        
executeCmdLoad :: SudokuBoard -> [String] -> String -> IO ()
executeCmdLoad sudokuBoard blockInfo prompt = do  
    putStrLn prompt 
    filePath <- getLine
    file <- readFile filePath
    let givenFile = lines file
        newBlockInfo = take (9) givenFile
        initialCellInfo = drop (9) givenFile
        newSudokuBoard = constructBoard newBlockInfo initialCellInfo
    printBoard newSudokuBoard
    sudokuGameLoop newSudokuBoard newBlockInfo

executeCmdMove :: SudokuBoard -> [String] -> MoveRecords -> MoveRecords -> String -> IO ()
executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo prompt = do
    putStrLn ""
    putStrLn prompt
    putStrLn "Available command \"stop\", \"undo\", \"redo\", \"hint\", \"board\", or press enter key to move "
    preMoveCmd <- getLine
    if preMoveCmd == "stop" 
        then do sudokuGameLoop sudokuBoard blockInfo
    else if preMoveCmd == "hint" 
        then do executeCmdHint sudokuBoard
                executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "board" 
        then do printBoard sudokuBoard
                executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "undo" 
        then do executeCmdUndo sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Please indicate the location and value."
    else if preMoveCmd == "redo" 
        then do executeCmdRedo sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Please indicate the location and value."
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
                            sudokuGameLoop newSudokuBoard blockInfo
                    else executeCmdMove newSudokuBoard blockInfo (movesToBeUndo++[(colNo, rowNo, value)]) movesToBeRedo "Please indicate the location and value."
            else if not $ isBlank sudokuBoard colNo rowNo
                then executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Conflict! There point is already been filled! Choose another point!"
            else if (not $ isRowValid sudokuBoard rowNo value) && (isColValid sudokuBoard colNo value)
                then executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Conflict! This number has already existed in this row! Please try again!"
            else if (isRowValid sudokuBoard rowNo value) && (not $ isColValid sudokuBoard colNo value)
                then executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Conflict! This number has already existed in this column! Please try again!"
            else if (not $ isRowValid sudokuBoard rowNo value) && (not $ isColValid sudokuBoard colNo value)
                then executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Conflict! This number has already existed in both row and column! Please try again!"
            else if (not $ isBlockValid sudokuBoard colNo rowNo value)
                then executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Conflict! This number has already existed in this block! Please try again!"
            else executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Sorry, there is a conflict!"
    else executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo "Command not correct, try again!"

executeCmdSave :: SudokuBoard -> [String] -> String -> IO ()
executeCmdSave sudokuBoard blockInfo prompt = do  
    putStrLn prompt
    filePath <- getLine
    saveToFile filePath sudokuBoard blockInfo
    putStrLn ("Saved successfully to file "++filePath++"!")
    sudokuGameLoop sudokuBoard blockInfo

executeCmdQuit :: SudokuBoard -> [String] -> IO ()
executeCmdQuit sudokuBoard blockInfo = do  
    confirmQuitGame sudokuBoard blockInfo
  
confirmQuitGame :: SudokuBoard -> [String] -> IO ()
confirmQuitGame sudokuBoard blockInfo = do
    putStrLn "Are you sure to quit?(y/n)"
    willQuit <- getLine
    if willQuit == "n"
        then sudokuGameLoop sudokuBoard blockInfo
    else if willQuit == "y"
        then confirmSaveGame sudokuBoard blockInfo
    else confirmQuitGame sudokuBoard blockInfo    

confirmSaveGame :: SudokuBoard -> [String] -> IO ()
confirmSaveGame sudokuBoard blockInfo = do 
    putStrLn "Do you want to save?(y/n)"
    willSave <- getLine 
    if willSave == "n"
        then do putStrLn "Quited without saving!"
    else if willSave == "y"
        then do putStrLn "Please indicate the file name to be saved"
                filePath <- getLine
                saveToFile filePath sudokuBoard blockInfo
                putStrLn ("Quited with the game being saved to file" ++ filePath)
    else confirmSaveGame sudokuBoard blockInfo

executeCmdUndo :: SudokuBoard -> [String] -> MoveRecords -> MoveRecords -> String -> IO ()
executeCmdUndo sudokuBoard blockInfo movesToBeUndo movesToBeRedo prompt = do
    if length movesToBeUndo == 0 
        then do 
            putStrLn "There is no action to be undo"
            executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo prompt
    else do
        let (colNo, rowNo, value) = last movesToBeUndo
            newSudokuBoard = updateBoard colNo rowNo 0 sudokuBoard
        putStrLn "\nNew board:" 
        printBoard newSudokuBoard 
        executeCmdMove newSudokuBoard blockInfo (init movesToBeUndo) (movesToBeRedo++[last movesToBeUndo]) prompt

executeCmdRedo :: SudokuBoard -> [String] -> MoveRecords -> MoveRecords -> String -> IO ()
executeCmdRedo sudokuBoard blockInfo movesToBeUndo movesToBeRedo prompt = do
    if length movesToBeRedo == 0 
        then do 
            putStrLn "There is no action to be redo"
            executeCmdMove sudokuBoard blockInfo movesToBeUndo movesToBeRedo prompt
    else do
        let (colNo, rowNo, value) = last movesToBeRedo
            newSudokuBoard = updateBoard colNo rowNo value sudokuBoard
        putStrLn "\nNew board:" 
        printBoard newSudokuBoard 
        executeCmdMove newSudokuBoard blockInfo (movesToBeUndo++[last movesToBeRedo]) (init movesToBeRedo) prompt 

executeCmdSolver :: SudokuBoard -> [String] -> IO ()        
executeCmdSolver sudokuBoard blockInfo = do
    putStrLn "Generating a possible answer, it may take longer time if there is 50+ blank cells, please wait..."
    let (hasAnswer, board) = mySolver sudokuBoard
    if hasAnswer then do printBoard board else do putStrLn "The current board is unsolveable!"

executeCmdHint :: SudokuBoard -> IO ()   
executeCmdHint sudokuBoard = do
    putStrLn ("You can try to fill the cell at column-" ++ [intToDigit $ getColNo nextCell] ++ " row-" ++ [intToDigit $ getRowNo nextCell] ++ " with the possible number " ++ possibleValsStr)
    where 
        possibleValsStr = [ intToDigit value | value <- possibleVals ]
        possibleVals = getPossibleVals sudokuBoard nextCell
        nextCell = getFirstBlank sudokuBoard

executeCmdNew sudokuBoard blockInfo = do
    numOfEmptyCells <- askLevel
    newSudokuBoard <- generateRandomSudokoBoard numOfEmptyCells
    printBoard newSudokuBoard
    sudokuGameLoop newSudokuBoard blockInfo
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

executeCmdHelp = undefined 
