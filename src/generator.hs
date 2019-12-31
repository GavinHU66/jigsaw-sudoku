module Generator where

import Cell
import Utils

-- Description: 
    -- Generate a random sudoku board.
-- Input:
    -- n: the number of empty cells in the randomly 
    --    generated sudoku board, indicating the level of difficulty
-- Output: 
    -- a IO SudokuBoard object, representing the 
    -- newly generated random sudoku board.
randBoard :: Int -> IO SudokuBoard
randBoard n = do
    file <- readFile "../maps/sample_sol.txt"
    let board = fileToBoardConverter file
    board' <- randFullBoard board
    emptyBoardCells board' n

-- Description: 
    -- Generate a full random sudoku board.
-- Input:
    -- board: the sudoku board to be filled with
-- Output: 
    -- a IO SudokuBoard object, representing the newly 
    -- generated random sudoku board.
randFullBoard :: SudokuBoard -> IO SudokuBoard
randFullBoard board = do 
    randList <- getRandList 9 1 9 True
    return $ swicthVals board randList

-- Description: 
    -- Swtich values in a sudoku board.
-- Input:
    -- board: the sudoku board to be filled with
    -- randList: indicate the rule for switching
-- Output: 
    -- a IO SudokuBoard object, representing the newly 
    -- generated random sudoku board.
swicthVals :: SudokuBoard -> [Int] -> SudokuBoard
swicthVals board randList = 
    map (\row -> updateRow row) board
    where 
        updateRow row = map (\cell -> updateCell cell) row
        updateCell (Cell colNo rowNo blockNo value) = Cell colNo rowNo blockNo (randList !! (value-1))
    

-- Description: 
    -- Fill a sudoku board with n zero values randomly.
-- Input:
    -- board: the sudoku board to be filled with
    -- n: the number of empty cells
-- Output: 
    -- a IO SudokuBoard object, representing the newly 
    -- generated random sudoku board.
emptyBoardCells :: SudokuBoard -> Int -> IO SudokuBoard
emptyBoardCells board n = do
    coords <- randomCoords n
    return $ map (\row -> map (\cell -> updateCell cell coords) row ) board
        where
            updateCell (Cell colNo rowNo blockNo value) coords
                | (colNo, rowNo) `elem` coords = Cell colNo rowNo blockNo 0
                | otherwise = Cell colNo rowNo blockNo value
  

-- Description: 
    -- Generate a random list of distinct coordinates.
-- Input:
    -- n: the number of coordinates in the randomly generated coordinates
-- Output: 
    -- a IO [(Int, Int)] object, representing the newly generated random list of distinct coordinates.
randomCoords :: Int -> IO [(Int, Int)]
randomCoords n = 
    randomCoords' n []
    where 
        randomCoords' n xs
            | length xs == n = return xs
            | otherwise = do
                colNo <- getRandNum 0 8
                rowNo <- getRandNum 0 8
                if ((colNo, rowNo) `elem` xs) then randomCoords' n xs else randomCoords' n (xs++[(colNo, rowNo)])


{-
    Below is an another algorithm to generate a new random sudoku board,
    with a random value and block pattern, due to the time-comsuming property
    of testing a newly generated random sudoku board is solveable, it is not
    practical in the run time

    step1 = [
        [((3,0),(3,1),0),((3,0),(4,0),0),((3,1),(4,1),0)],
        [((6,0),(7,0),1),((6,0),(6,1),1),((6,1),(7,1),1)],
        [((7,3),(8,3),2),((7,3),(7,4),2),((8,3),(8,4),2)],
        [((7,6),(8,6),5),((7,6),(7,7),5),((8,6),(8,7),5)],
        [((4,7),(5,7),8),((4,8),(5,8),8),((5,7),(5,8),8)],
        [((1,7),(2,7),7),((1,8),(2,8),7),((2,7),(2,8),7)],
        [((0,5),(1,5),6),((0,4),(0,5),6),((1,4),(1,5),6)],
        [((0,2),(1,2),3),((0,1),(0,2),3),((1,1),(1,2),3)]]
    centralStep1 = [
        [[(3,2,4),(5,3,1)],[(5,6,4),(3,5,7)]],
        [[(6,3,4),(5,5,5)],[(2,5,4),(3,3,3)]]]

    step2 = [
        [((0,3),(1,3),0),((0,3),(0,4),0),((1,3),(1,4),0)],
        [((0,6),(1,6),3),((0,6),(0,7),3),((1,6),(1,7),3)],
        [((3,7),(3,8),6),((3,7),(4,7),6),((3,8),(4,8),6)],
        [((6,7),(6,8),7),((6,7),(7,7),7),((6,8),(7,8),7)],
        [((7,5),(8,5),8),((7,4),(7,5),8),((8,4),(8,5),8)],
        [((7,2),(8,2),5),((7,1),(7,2),5),((8,1),(8,2),5)],
        [((5,0),(5,1),2),((4,0),(5,0),2),((4,1),(5,1),2)],
        [((2,0),(2,1),1),((1,0),(2,0),1),((2,1),(1,1),1)]]
    centralStep2 = [
        [[(5,2,4),(3,3,1)],[(3,6,4),(5,5,7)]],
        [[(2,3,4),(3,5,3)],[(6,5,4),(5,3,5)]]]

    generateRandomBoard :: Int -> IO SudokuBoard
    generateRandomBoard n = do
        file <- readFile "map6.txt"
        direction <- generateRandomNumBetween '0' '1'
        choicesIdx <- generateRandomList 8 '0' '2' False
        centralIdx <- generateRandomNumBetween '0' '1'
        centralIdx' <- generateRandomNumBetween '0' '2'
        let board = fileToBoardConverter file
            step = if direction == 0 then step1 else step2
            centralStep = if direction == 0 then centralStep1 else centralStep2
            centralMove = if centralIdx' == 2 then concat (centralStep !! centralIdx) else (centralStep !! centralIdx) !! centralIdx'
            sequence = [ choice !! idx | (choice, idx) <- zip step choicesIdx ]
            sequence' = concat [ [(c1,r1,b), (c2,r2,b)] | ((c1,r1),(c2,r2),b) <- sequence ] ++ centralMove
            emptySodukoBoard = updateBoardBlocks board sequence'
        newSodukoBoard <- fillEmptyBoard n emptySodukoBoard

        return newSodukoBoard
        -- let (hasAnswer, board) = mySolver newSodukoBoard
        -- if hasAnswer then return newSodukoBoard else generateRandomBoard n

    fillEmptyBoard :: Int -> SudokuBoard -> IO SudokuBoard    
    fillEmptyBoard n board
        | n == 0 = return board
        | otherwise = do
            colNo <- generateRandomNumBetween '0' '8'
            rowNo <- generateRandomNumBetween '0' '8'
            val <- generateRandomNumBetween '1' '9'
            if isValidMove colNo rowNo val board 
                then fillEmptyBoard (n-1) (updateBoard colNo rowNo val board)
            else fillEmptyBoard n board

    updateBoardBlocks :: SudokuBoard -> [(Int, Int, Int)] -> SudokuBoard        
    updateBoardBlocks board coords =
        map (\row -> updateRow row coords) board
            where
                updateRow row coords = map (\cell -> updateCell cell coords) row
                updateCell (Cell colNo rowNo blockNo val) coords
                    | (colNo, rowNo) `elem` [ (colNo, rowNo) | (colNo, rowNo, blockNo) <- coords] = Cell colNo rowNo (head $ [ blockNo | (colNo', rowNo', blockNo) <- coords, colNo == colNo' && rowNo == rowNo']) val
                    | otherwise = Cell colNo rowNo blockNo val
-}



