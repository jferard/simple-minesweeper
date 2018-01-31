module Game where

import BaseTypes
import UI
import Logic
import Data.Char

-- this is the game : clear the screen, draw the board and wait for
game :: (Monad m, UI a m) => a -> [[Int]] -> [[Cell]] -> m ()
game ui bombGrid maskGrid = do
    renderInitialBoard ui bombGrid maskGrid
    case gameState bombGrid maskGrid of
        Win -> renderWin ui bombGrid maskGrid
        Loss -> renderWin ui bombGrid maskGrid
        Playing -> do
            commandLine <- getCommandLine ui
            let (colChar:rowChar:command) = commandLine
            let c = ord(colChar) - ord('a')
            let r = ord(rowChar) - ord('0')
            case command of
                [] -> game ui bombGrid (if (maskGrid !! r) !! c == Masked then unmask bombGrid maskGrid (r, c) else set maskGrid (r, c) Masked)
                ('x':_) -> game ui bombGrid (set maskGrid (r, c) Cross)
                ('?':_) -> game ui bombGrid (set maskGrid (r, c) Question)