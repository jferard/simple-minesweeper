module Logic where

import System.Random
import BaseTypes

-- draw the board game : a board is the superposition of two grids. If the cell is unmasked, then show the bomb grid, else show the current mask
instance Show Board where
    show (Board bombGrid maskGrid) =
        init $ ' ':['A'..'J'] ++ "\n" ++ concatMap showRowOfBoard (zip3 [0..] bombGrid maskGrid)
        where showRowOfBoard (i, bombRow, maskRow) = (show i ++ concatMap showCell (zip bombRow maskRow)) ++ "\n"
                                                where
                                                    showCell :: (Int, Cell) -> String
                                                    showCell (b, m) = case (b, m) of
                                                                    (_, Masked) -> "-"
                                                                    (_, Question) -> "?"
                                                                    (_, Cross) -> "X"
                                                                    (0, Unmasked) -> " "
                                                                    (-1, Unmasked) -> "B"
                                                                    (x, Unmasked) -> show x

cellIsMasked :: Board -> (Int, Int) -> Bool
cellIsMasked (Board _ maskGrid) (r, c) = (maskGrid !! r) !! c == Masked

-- unmask a cell, and the adjacent cells if there is no bomb around.
unmaskCell :: Board -> (Int, Int) -> Board
unmaskCell board@(Board bombGrid  maskGrid) (r, c) =
    let newBoard@(Board _ newMaskGrid) = setCell board (r, c) Unmasked in
    case (bombGrid !! r) !! c of
        -- if it's a "0" cell, then unmask all masked neighbors
        0 -> foldl unmaskCell newBoard maskedNeighbors
            where
                maskedNeighbors :: [(Int, Int)]
                maskedNeighbors = filter (\ (nr,nc) -> 0 <= nr && nr < 10 && 0<= nc && nc < 10 && (newMaskGrid !! nr) !! nc /= Unmasked) allNeighbors
                allNeighbors = [(r+dr, c+dc) | dr<-[-1,0,1], dc<-[-1,0,1]]
        _ -> newBoard


createBoard :: Int -> Int -> [(Int, Int)] -> Board
createBoard h w bombsCoordinates = Board (createBombGrid h w bombsCoordinates) (createMaskGrid h w)
    where
        -- given a height and a width, create a bomb grid : -1 is a bomb, n >=0 is the number of bombs in the neighborhood
        createBombGrid :: Int -> Int -> [(Int, Int)] -> [[Int]]
        createBombGrid h w bombsCoordinates = [createBombRow r w bombsCoordinates | r <- [0..h-1]]
        createBombRow r w bombsCoordinates = map f [0..(w-1)]
            where f c =
                    if (r,c) `elem` bombsCoordinates
                    then -1
                    else
                        foldl g 0 bombsCoordinates
                            where g acc (rBomb, cBomb) = if r-rBomb `elem` [-1,0,1] && c-cBomb `elem` [-1,0,1] then acc+1 else acc


        -- given a height and a width, create a mask grid
        createMaskGrid :: Int -> Int -> [[Cell]]
        createMaskGrid h w = [[Masked | c <- [0..(w-1)]]| r <- [0..h-1]]


-- algorithm : start with a win. If a bomb is unmasked, result is a loss. If a non bomb is masked, the result is playing
gameState :: Board -> State
gameState (Board bombGrid maskGrid) = foldl gameOverRow Win (zip bombGrid maskGrid)
    where
        gameOverRow :: State -> ([Int], [Cell]) -> State
        gameOverRow Loss _ = Loss
        gameOverRow state (bombRow, maskRow) = foldl updateGameOver state (zip bombRow maskRow)

        updateGameOver :: State -> (Int, Cell) -> State
        updateGameOver Loss _ = Loss
        updateGameOver state bm = case bm of
            (-1, Unmasked) -> Loss
            (b, m) | b >= 0 && m /= Unmasked -> Playing
            _ -> state

-- set a value in a grid
setCell :: Board -> (Int, Int) -> Cell -> Board
setCell (Board bombGrid maskGrid) (r, c) value = Board bombGrid (set' maskGrid (r, c) value)
    where
        set' :: [[a]] -> (Int, Int) -> a -> [[a]]
        set' grid (r, c) value = befRows ++ [setInRow row c value] ++ aftRows
            where
                (befRows, row:aftRows) = splitAt r grid
                setInRow row c value = befCells ++ [value] ++ aftCells
                    where
                        (befCells, _:aftCells) = splitAt c row

-- given a StdGen and a count, return n coordinates. May loop forever.
createBombsCoordinates :: StdGen -> Int -> [(Int, Int)]
{-
type StdGen = Int
createBombsCoordinates x n = [(1,2), (4,6), (4,7), (5,5), (9,8), (3,3), (4,2)]
-}
createBombsCoordinates gen' count = createBombsCoordinates' gen' count []
    where
    createBombsCoordinates' :: StdGen -> Int -> [(Int, Int)] -> [(Int, Int)]
    createBombsCoordinates' _ 0 coords = coords
    createBombsCoordinates' g n coords = if (r, c) `elem` coords then createBombsCoordinates' g'' n coords else createBombsCoordinates' g'' (n-1) ((r, c):coords)
        where
            r :: Int
            c :: Int
            (r, g') = randomR (0,9) g
            (c, g'') = randomR (0,9) g'


