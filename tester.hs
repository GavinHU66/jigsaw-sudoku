import System.Random
import Data.Char
import Data.List
import RenderBoard
import Cell
import Utils

import Control.Exception (try)
import GHC.IO.Exception (IOException(..))
import qualified Data.ByteString as B

main :: IO ()
main = do
    putStrLn "please enter file path (e.g., \"./YourPath/map.txt\", or file name if it\'s in the same folder): "
    filePath <- getLine
    file <- readFile filePath
    let sudokuBoard = fileToBoardConverter file
        file2 = boardToFileConverter sudokuBoard
    putStrLn $ unlines $ file2

printException :: Either IOError a -> IO Bool
printException (Right _) = do putStrLn "No exception caught"; return True
printException (Left e) = do  putStrLn $ concat [ "ioe_filename = "
                                            , show $ ioe_filename e
                                            , ", ioe_description = "
                                            , show $ ioe_description e
                                            , ", ioe_errno = "
                                            , show $ ioe_errno e
                                            ]; return False
myfunc = try (writeFile "/dev/full" " ") >>= printException