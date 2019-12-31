import RenderBoard
import Command
import Cell
import Utils

-- Description: 
    -- The entry of the whole game,
    -- Simply enter main in the terminal after ghci is launched.
main :: IO ()
main = do
    file <- safeRF
    let board = fileToBoardConverter file
    printBoard board
    gameLoop board False

-- Description: 
    -- The loop body of the whole game,
    -- Simply enter main in the terminal after ghci is launched.
-- Output: 
    -- a IO () object
gameLoop :: SudokuBoard -> Bool -> IO ()
gameLoop board shouldTerminate
    | shouldTerminate = putStrLn "\nGame is quited! Hope to see you later!\n"
    | otherwise = do 
        putStrLn ""
        cmd <- askCmd "Please enter your command, enter \"help\" to view all commands, to get started, enter \"move\"!"
        (newBoard, shouldTerminate) <- exeCmd cmd board
        gameLoop newBoard shouldTerminate