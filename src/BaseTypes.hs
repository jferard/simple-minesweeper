module BaseTypes where

-- cells are masked (blank, or with a question mark or a cross) or unmasked.
data Cell = Masked | Question | Cross | Unmasked
    deriving (Eq)

data State = Win | Loss | Playing

data Board = Board [[Int]] [[Cell]]