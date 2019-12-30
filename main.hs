import RenderBoard
import Command
import Cell
import Utils

main :: IO ()
main = do
    putStrLn "please enter file path (e.g., \"./YourPath/map.txt\", or file name if it\'s in the same folder): ";
    filePath <- getLine;
    file <- readFile filePath;
    let sudokuBoard = fileToBoardConverter file
    printBoard sudokuBoard
    gameLoop sudokuBoard False

gameLoop :: SudokuBoard -> Bool -> IO ()
gameLoop sudokuBoard shouldTerminate
    | shouldTerminate = putStrLn "Quited!"
    | otherwise = do 
        putStrLn ""
        cmd <- askCmd "please enter your command, enter help to view all commands, to get started, type in \"move\"!"
        (newSudokuBoard, shouldTerminate) <- executeCmd cmd sudokuBoard
        gameLoop newSudokuBoard shouldTerminate