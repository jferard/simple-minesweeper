-- simple minesweeper
-- (C) J. Férard (https://github.com/jferard) 2017
-- A simple minesweeper in Haskell
-- The code takes less than 150 lines (including comments and signatures: this is no a code golf contest!)
-- LICENSE : GPL v3

import System.Process
import Data.Char
import GHC.IO.Exception
import System.Random

-- cells are masked (blank, or with a question mark or a cross) or unmasked. 
data Cell = Masked | Question | Cross | Unmasked 
    deriving (Eq)
    
data State = Win | Loss | Playing

-- draw the board game : a board is the superposition of two grids. If the cell is unmasked, then show the bomb grid, else show the current mask
drawBoard::[[Int]] -> [[Cell]] -> [[String]]
drawBoard bombGrid maskGrid = map drawRowOfBoard (zip bombGrid maskGrid)
    where drawRowOfBoard (bombRow, maskRow) = map drawCell (zip bombRow maskRow)
                                                where 
                                                    drawCell :: (Int, Cell) -> String
                                                    drawCell (b, m) = case (b, m) of 
                                                                    (_, Masked) -> "-"
                                                                    (_, Question) -> "?"
                                                                    (_, Cross) -> "X"
                                                                    (0, Unmasked) -> " "
                                                                    (-1, Unmasked) -> "B"
                                                                    (x, Unmasked) -> show x

-- clear the screen (Hack for Un*x, Windows)
clearScreen :: IO ()
clearScreen = do  
    _ <- system "clear"
    _ <- system "cls"
    return()

-- print the board, with a..j and 0..9    
printBoard::[[Int]] -> [[Cell]] -> IO [()]
printBoard bombGrid maskGrid = do
    putStrLn $ ' ':['A'..'J']
    mapM (\ (i, row) -> putStrLn $ show i++(concat row)) (zip [0..] (drawBoard bombGrid maskGrid))

-- Nothing : the game is not over, Just True : win, Just False loose    
-- algorithm : start with a win. If a bomb is unmasked, result is a loss. If a non bomb is masked, the result is playing
gameState :: [[Int]] -> [[Cell]] -> State
gameState bombGrid maskGrid = foldl gameOverRow Win (zip bombGrid maskGrid)
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

-- this is the game : clear the screen, draw the board and wait for 
game :: [[Int]] -> [[Cell]] -> IO ()
game bombGrid maskGrid = do
    clearScreen
    printBoard bombGrid maskGrid
    case gameState bombGrid maskGrid of
        Win -> putStrLn "Board Complete"
        Loss -> putStrLn "BOOM!!!"
        Playing -> do 
            putStrLn "Enter command CR-"
            commandLine <- (map toLower) <$> getLine
            let (colChar:rowChar:command) = commandLine
            let c = ord(colChar) - ord('a')
            let r = ord(rowChar) - ord('0')
            case command of
                [] -> game bombGrid (if (maskGrid !! r) !! c == Masked then unmask bombGrid maskGrid (r, c) else set maskGrid (r, c) Masked)
                ('x':_) -> game bombGrid (set maskGrid (r, c) Cross)
                ('?':_) -> game bombGrid (set maskGrid (r, c) Question)
                
-- unmask a cell, and the adjacent cells if there is no bomb around.                
unmask :: [[Int]] -> [[Cell]] -> (Int, Int) -> [[Cell]]
unmask bombGrid  maskGrid (r, c) = let grid = set maskGrid (r, c) Unmasked in case (bombGrid !! r) !! c of
    0 -> foldl (unmask bombGrid) grid (neighbors grid)
        where 
            allNeighbors = [(r+dr, c+dc) | dr<-[-1,0,1], dc<-[-1,0,1]]
            neighbors grid = filter (\ (nr,nc) -> 0 <= nr && nr < 10 && 0<= nc && nc < 10 && (grid !! nr) !! nc /= Unmasked) allNeighbors
    _ -> grid

-- set a value in a grid    
set :: [[a]] -> (Int, Int) -> a -> [[a]]
set grid (r, c) value = befRows ++ [setInRow row c value] ++ aftRows
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

-- let's start.
main :: IO ()    
main = do 
    gen <- getStdGen
    let (count, gen') = randomR (10,30) gen
{-
    let gen' = 0 -- temp
    let count = 0 -- temp
-}
    let bombsCoordinates = createBombsCoordinates gen' count 
    game (createBombGrid 10 10 bombsCoordinates) (createMaskGrid 10 10)
