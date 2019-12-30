import RenderBoard
import Command
import Cell
import Utils

main :: IO ()
main = do
    file <- safeRF
    let sudokuBoard = fileToBoardConverter file
    printBoard sudokuBoard
    gameLoop sudokuBoard False

gameLoop :: SudokuBoard -> Bool -> IO ()
gameLoop sudokuBoard shouldTerminate
    | shouldTerminate = putStrLn "\nGame is quited! Hope to see you later!\n"
    | otherwise = do 
        putStrLn ""
        cmd <- askCmd "Please enter your command, enter \"help\" to view all commands, to get started, enter \"move\"!"
        (newSudokuBoard, shouldTerminate) <- executeCmd cmd sudokuBoard
        gameLoop newSudokuBoard shouldTerminate