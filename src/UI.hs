{-# LANGUAGE MultiParamTypeClasses #-}

module UI where

import BaseTypes

class Monad m => UI a m where
    -- may clear the screen
    nextStep :: a -> m ()

    -- get the user command
    getCommandLine :: a -> m String

    -- render the initial board
    renderInitialBoard :: a -> [[Int]] -> [[Cell]] -> m ()

    -- render a win
    renderWin :: a -> [[Int]] -> [[Cell]] -> m ()

    -- render a loss
    renderLoss :: a -> [[Int]] -> [[Cell]] -> m ()

    -- render the board after the cell r, c changed
    -- may re-render the whole board or only the impacted cells
    renderBoard :: a -> [[Int]] -> [[Cell]] -> Int -> Int -> m ()
    renderBoard ui bombGrid maskGrid r c = renderInitialBoard ui bombGrid maskGrid