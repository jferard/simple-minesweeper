{-# LANGUAGE MultiParamTypeClasses #-}

module TUI where

import GHC.IO.Exception
import System.Process
import BaseTypes
import Data.Char
import Logic
import UI

data TUI = TUI

instance UI TUI IO where
    nextStep tui = do
        clearScreen

    getCommandLine tui = (map toLower) <$> getLine

    renderInitialBoard tui bombGrid maskGrid = do
        clearScreen
        printBoard bombGrid maskGrid
        where
            -- print the board, with a..j and 0..9
            printBoard::[[Int]] -> [[Cell]] -> IO ()
            printBoard bombGrid maskGrid = do
                putStrLn $ ' ':['A'..'J']
                mapM_ (\ (i, row) -> putStrLn $ show i++(concat row)) (zip [0..] (showBoard bombGrid maskGrid))

    renderWin tui bombGrid maskGrid = do
        putStrLn "Board Complete"

    renderLoss tui bombGrid maskGrid = do
        putStrLn "BOOM!!!"

-- clear the screen (Hack for Un*x, Windows)
clearScreen :: IO ()
clearScreen = do
    _ <- system "clear"
    _ <- system "cls"
    return()
