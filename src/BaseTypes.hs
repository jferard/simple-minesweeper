module BaseTypes where

import Graphics.UI.Gtk as Gtk

-- cells are masked (blank, or with a question mark or a cross) or unmasked.
data Tile = Masked | Question | Cross | Unmasked
    deriving (Eq)

data State = Win | Loss | Playing

data Board = Board [[Int]] [[Tile]]

data Context = Context DrawingArea Double Board
