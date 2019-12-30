import RenderBoard
import Command
import Utils

main :: IO ()
main = 
    do {
        putStrLn "please enter file path (e.g., \"./YourPath/map.txt\", or file name if it\'s in the same folder): ";
        filePath <- getLine;
        file <- readFile filePath;
        printBoard (fileToBoardConverter file);
        sudokuGameLoop (fileToBoardConverter file) (take (9) (lines file));
    }