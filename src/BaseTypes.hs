module BaseTypes where

import Graphics.UI.Gtk as Gtk

-- cells are masked (blank, or with a question mark or a cross) or unmasked.
data Tile = Masked | Question | Cross | Unmasked
    deriving (Eq)

data State = Win | Loss | Playing

data Cell = Cell { bomb :: Int, tile :: Tile }

data Board = Board [[Cell]]

data Context = Context DrawingArea Double Board

-- draw the board game : a board is the superposition of two grids. If the cell is unmasked, then show the bomb grid, else show the current mask
instance Show Board where
    show (Board rows) =
        init $ ' ':['A'..'J'] ++ "\n" ++ concatMap showRowOfBoard (zip [0..] rows)
            where showRowOfBoard (i, row) = show i ++ concatMap show row ++ "\n"

-- If the cell is unmasked, then show the bomb grid, else show the current mask
instance Show Cell where
    show Cell{bomb = b, tile=t} = case (b, t) of
                                    (_, Masked) -> "-"
                                    (_, Question) -> "?"
                                    (_, BaseTypes.Cross) -> "X"
                                    (0, Unmasked) -> " "
                                    (-1, Unmasked) -> "B"
                                    (x, Unmasked) -> show x
