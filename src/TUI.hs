{-# LANGUAGE MultiParamTypeClasses #-}

module TUI where

import GHC.IO.Exception
import System.Process
import BaseTypes
import Data.Char
import Logic

data TUI = TUI

nextStep :: IO ()
nextStep = do
    clearScreen

getCommandLine :: IO String
getCommandLine = (map toLower) <$> getLine


renderBoard :: Board -> IO ()
renderBoard board = do
    clearScreen
    putStrLn $ show board

renderWin :: Board -> IO ()
renderWin board = do
    putStrLn "Board Complete"

renderLoss :: Board -> IO ()
renderLoss board = do
    putStrLn "BOOM!!!"

-- clear the screen (Hack for Un*x, Windows)
clearScreen :: IO ()
clearScreen = do
    _ <- system "clear"
    _ <- system "cls"
    return()

-- this is the game : clear the screen, draw the board and wait for
game :: Board -> IO ()
game board = do
    renderBoard board
    case gameState board of
        Win -> renderWin board
        Loss -> renderLoss board
        Playing -> do
            commandLine <- getCommandLine
            let (colChar:rowChar:command) = commandLine
            let c = ord(colChar) - ord('a')
            let r = ord(rowChar) - ord('0')
            case command of
                [] -> game (if cellIsMasked board (r, c) then unmaskCell board (r, c) else setTile board (r, c) Masked)
                ('x':_) -> game (setTile board (r, c) Cross)
                ('?':_) -> game (setTile board (r, c) Question)