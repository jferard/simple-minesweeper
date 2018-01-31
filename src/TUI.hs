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

    renderInitialBoard tui board = do
        clearScreen
        putStrLn $ show board

    renderWin tui board = do
        putStrLn "Board Complete"

    renderLoss tui board = do
        putStrLn "BOOM!!!"

-- clear the screen (Hack for Un*x, Windows)
clearScreen :: IO ()
clearScreen = do
    _ <- system "clear"
    _ <- system "cls"
    return()
