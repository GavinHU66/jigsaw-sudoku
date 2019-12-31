import System.Random
import Data.Char
import Data.List
import RenderBoard
import Cell
import Utils

import Control.Exception (try)
import GHC.IO.Exception (IOException(..))
import qualified Data.ByteString as B

-- main :: IO ()
main = do

    
    file <- readFile "../maps/map3.txt"
    let board = fileToBoardConverter file
        cList = colList board
        rList = rowList board
        bList = blockList board
    putStrLn (show cList)
    putStrLn (show rList)
    putStrLn (show bList)
    let possVals = getPossVals 2 0 1 cList rList bList
    putStrLn (show possVals)
    let cList' = addValLists cList 2 1
        rList' = addValLists rList 0 1
        bList' = addValLists bList 1 1
    putStrLn (show cList')
    putStrLn (show rList')
    putStrLn (show bList')
    let cList'' = removeValLists cList 2 1
        rList'' = removeValLists rList 0 1
        bList'' = removeValLists bList 1 1
    putStrLn (show cList'')
    putStrLn (show rList'')
    putStrLn (show bList'')


-- addValLists xss idx val = 
--     [ if i==idx then xs++[val] else xs | (xs, i) <- zip xss [0..8] ]

-- removeValLists xss idx val = 
--     [ if i==idx then [x|x<-xs,x/= val] else xs | (xs, i) <- zip xss [0..8] ]