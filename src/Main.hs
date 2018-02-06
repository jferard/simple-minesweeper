-- simple minesweeper
-- (C) J. Férard (https://github.com/jferard) 2017
-- A simple minesweeper in Haskell
-- The code takes less than 150 lines (including comments and signatures: this is no a code golf contest!)
-- LICENSE : GPL v3

import System.Random
import Logic
import TUI
import GUI
import UI

-- let's start.
main :: IO ()    
main = do
    board <- initBoard 10 10 10
    GUI.initGame GUI 25 board