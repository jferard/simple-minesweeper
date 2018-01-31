{-# LANGUAGE MultiParamTypeClasses #-}

module UI where

import BaseTypes

class Monad m => UI a m where
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