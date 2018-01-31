module Game where

import BaseTypes
import UI
import Logic
import Data.Char

-- this is the game : clear the screen, draw the board and wait for
game :: (Monad m, UI a m) => a -> Board -> m ()
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
                [] -> game ui (if cellIsMasked board (r, c) then unmaskCell board (r, c) else setCell board (r, c) Masked)
                ('x':_) -> game ui (setCell board (r, c) Cross)
                ('?':_) -> game ui (setCell board (r, c) Question)