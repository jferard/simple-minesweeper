-- simple minesweeper
-- (C) J. Férard (https://github.com/jferard) 2017
-- A simple minesweeper in Haskell
-- The code takes less than 150 lines (including comments and signatures: this is no a code golf contest!)
-- LICENSE : GPL v3

import System.Random
import System.Environment
import Logic
import TUI
import GUI

-- let's start.
main :: IO ()    
main = do
    board <- initBoard 10 10 10
    args <- getArgs
    let ui = case args of
                ["tui"] -> "tui"
                _ -> "gui"

    if ui == "tui" then TUI.game board else GUI.initGame GUI 10 board
