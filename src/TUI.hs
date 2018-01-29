module TUI where

import GHC.IO.Exception
import System.Process
import BaseTypes
import Data.Char
import Logic

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
    mapM (\ (i, row) -> putStrLn $ show i++(concat row)) (zip [0..] (showBoard bombGrid maskGrid))

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
