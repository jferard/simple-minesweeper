{-# LANGUAGE MultiParamTypeClasses #-}

module UI where

import BaseTypes
import Logic
import Data.Char

class Monad m => UI a m where
    -- init the game
    initGame :: a -> m()

    -- may clear the screen
    nextStep :: a -> m ()

    -- get the user command
    getCommandLine :: a -> m String

    -- render the initial board
    renderInitialBoard :: a -> Board -> m ()

    -- render a win
    renderWin :: a -> Board -> m ()

    -- render a loss
    renderLoss :: a -> Board -> m ()


    -- render the board after the cell r, c changed
    -- may re-render the whole board or only the impacted cells
    renderBoard :: a -> Board -> Int -> Int -> m ()
    renderBoard ui board _ _ = renderInitialBoard ui board


    -- this is the game : clear the screen, draw the board and wait for
    game :: a -> Board -> m ()
    game ui board = do
        renderInitialBoard ui board
        case gameState board of
            Win -> renderWin ui board
            Loss -> renderLoss ui board
            Playing -> do
                commandLine <- getCommandLine ui
                let (colChar:rowChar:command) = commandLine
                let c = ord(colChar) - ord('a')
                let r = ord(rowChar) - ord('0')
                case command of
                    [] -> game ui (if cellIsMasked board (r, c) then unmaskCell board (r, c) else setTile board (r, c) Masked)
                    ('x':_) -> game ui (setTile board (r, c) Cross)
                    ('?':_) -> game ui (setTile board (r, c) Question)