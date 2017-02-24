# Simple Minesweeper

A (very) simple minesweeper in Haskell, under GPL v3.

## Motivation
I had heard that Haskell was a very good language to write quick prototypes. I've written my first minesweeper in a couple of hours. It is very basic (all in ASCII), but it works fine.

## Let's play
``` 
...$ git clone https://github.com/jferard/simple-minesweeper.git 
...$ cd simple-minesweeper
...$ cabal install random
...$ runhaskell simple-minesweeper.hs
```

The command syntax is very simple:
* [A-J][0-9] : unmask the cell at the given position, or remove cross or question mark
* [A-J][0-9]x : set a cross at the given position
* [A-J][0-9]? : set a question mark at the given position

Good luck !!!
