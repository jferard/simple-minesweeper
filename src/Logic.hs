module Logic where

import System.Random
import BaseTypes
import Debug.Trace

initBoard :: Int -> Int -> Int -> IO Board
initBoard bombCount rows columns = do
    gen <- newStdGen
    let bombsCoordinates = createBombsCoordinates gen rows columns bombCount
    let board = createBoard rows columns bombsCoordinates
    return board

cellIsMasked :: Board -> (Int, Int) -> Bool
cellIsMasked (Board rows) (r, c) = tile ((rows !! r) !! c) == Masked

-- unmask a cell, and the adjacent cells if there is no bomb around.
unmaskCell :: Board -> (Int, Int) -> Board
unmaskCell board (r, c) =
    let newBoard@(Board rows) = setTile board (r, c) Unmasked
        b = bomb ((rows !! r) !! c) in
    case b of
        -- if it's a "0" cell, then unmask all masked neighbors
        0 -> foldl unmaskCell newBoard maskedNeighbors
            where
                maskedNeighbors :: [(Int, Int)]
                maskedNeighbors = filter (\ (nr,nc) ->
                                                    0 <= nr && nr < 10 &&
                                                    0 <= nc && nc < 10 &&
                                                    cellIsMasked newBoard (nr, nc)
                                         ) allNeighbors
                allNeighbors = [(r+dr, c+dc) | dr<-[-1,0,1], dc<-[-1,0,1]]
        _ -> newBoard


createBoard :: Int -> Int -> [(Int, Int)] -> Board
createBoard height width bombsCoordinates = Board (createRows height width bombsCoordinates)
    where
        -- given a height and a width, create a bomb grid : -1 is a bomb, n >=0 is the number of bombs in the neighborhood
        createRows :: Int -> Int -> [(Int, Int)] -> [[Cell]]
        createRows height width bombsCoordinates = [createRow r width bombsCoordinates | r <- [0..height-1]]
        createRow r width bombsCoordinates = map createCell [0..(width-1)]
            where
                createCell c = Cell {
                            bomb = if (r, c) `elem` bombsCoordinates then -1 else computeRisk c,
                            tile = Masked
                        }
                computeRisk c = foldl risk 0 bombsCoordinates
                    where risk acc (rBomb, cBomb) = if r-rBomb `elem` [-1,0,1] && c-cBomb `elem` [-1,0,1] then acc+1 else acc


-- algorithm : start with a win. If a bomb is unmasked, result is a loss. If a non bomb is masked, the result is playing
gameState :: Board -> State
gameState (Board rows) = foldl gameOverRow Win rows
    where
        gameOverRow :: State -> [Cell] -> State
        gameOverRow Loss _ = Loss
        gameOverRow state cells = foldl updateGameOver state cells

        updateGameOver :: State -> Cell -> State
        updateGameOver Loss _ = Loss
        updateGameOver state cell = case cell of
            Cell {bomb = -1, tile = Unmasked } -> Loss
            Cell {bomb = b, tile = t } | b >= 0 && t /= Unmasked -> Playing
            _ -> state

-- set a value in a grid
setTile :: Board -> (Int, Int) -> Tile -> Board
setTile (Board rows) (r, c) value = Board $ befRows ++ [setInRow row c value] ++ aftRows
            where
                (befRows, row:aftRows) = splitAt r rows
                setInRow :: [Cell] -> Int -> Tile -> [Cell]
                setInRow row c value = befCells ++ [cell { tile = value }] ++ aftCells
                    where
                        (befCells, cell:aftCells) = splitAt c row

-- given a StdGen and a count, return n coordinates
-- Fisher Yates shuffle
createBombsCoordinates :: StdGen -> Int -> Int -> Int -> [(Int, Int)]
createBombsCoordinates gen rows columns count =
    let
        size = rows * columns
        initialArr = map (\i -> i<count) [0..size] -- count Trues then Falses
    in convert $ shuffle gen (size - 1) initialArr
    where
    -- probably equivalent to System.Random.Shuffle.shuffle'
    shuffle :: StdGen -> Int -> [a] -> [a]
    shuffle _ 0 array = array
    shuffle g i array =
        let (j, g') = randomR (0,i) g
        in shuffle g' (i-1) (exchange j i array)
        where
            exchange :: Int -> Int -> [a] -> [a]
            exchange j i arr = zipWith (\k element -> if k == j then arr!!i else if k == i then arr!!j else element) [0..] arr
    convert :: [Bool] -> [(Int, Int)]
    convert arr = [divMod i columns | (i,b)<-zip [0..] arr, b]

