module GUI where

import BaseTypes
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo

printHello :: IO ()
printHello = do
    initGUI
    -- window
    window <- windowNew
    onDestroy window mainQuit
    set window [
       containerBorderWidth := 10,
       windowDefaultWidth := 350,
       windowTitle := "Simple Minesweeper (J. Férard)" ]

    -- mainbox
    mainbox <- vBoxNew False 10

    -- text
    msg <- labelNew (Just "A GTK version is under development.")
    containerAdd mainbox msg

    -- button
    button <- buttonNew
    set button [ buttonLabel := "Ok" ]
    onClicked button $ do
        widgetDestroy window
    containerAdd mainbox button

    -- frame
    frame <- frameNew
    containerAdd mainbox frame
    canvas <- drawingAreaNew
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
    containerAdd frame canvas
    widgetSetSizeRequest frame 350 350

    -- end mainbox
    containerAdd window mainbox
    widgetShowAll window

    _ <- widgetGetDrawWindow canvas
    on canvas exposeEvent $ renderBoard

    on canvas buttonPressEvent $ printXYButton

    mainGUI

printXYButton :: EventM EButton Bool
printXYButton = do
    (x, y)      <- eventCoordinates
    b <- eventButton
    liftIO $ do putStrLn $ show (x, y, b)

    return True

renderBoard :: EventM EExpose Bool
renderBoard = do
    dw      <- eventWindow
    region  <- eventRegion >>= liftIO . regionGetRectangles
    liftIO . renderWithDrawable dw $ do
        renderCell dw 0 0 1 Masked
        renderCell dw 0 1 1 Question
        renderCell dw 0 2 1 BaseTypes.Cross
        renderCell dw 0 3 0 Unmasked
        renderCell dw 0 4 (-1) Unmasked
        renderCell dw 0 5 5 Unmasked
    return True


renderCell :: DrawWindow -> Double -> Double -> Int -> Cell -> Render ()
renderCell = renderCellOfSize 20

renderCellOfSize :: Double -> DrawWindow -> Double -> Double -> Int -> Cell -> Render ()
renderCellOfSize size dw r c bomb mask = do
    renderTile size r c
    setSourceRGB 0 0 0
    moveTo (c*size+size*0.30) (r*size+size*0.75)
    showText $ showCell bomb mask

    where
        renderTile :: Double -> Double -> Double -> Render()
        renderTile size r c =
            let x = c*size
                y = r*size
            in do
                liftIO $ do drawWindowClearArea dw (floor x) (floor y) (ceiling $ x+size-1) (ceiling $ y+size-1)
                setLineWidth 2

                setSourceRGB 0 0 0
                moveTo (x+size-1) y
                lineTo x y
                lineTo x (y+size-1)
                stroke

                setSourceRGB 0.5 0.5 0.5
                moveTo x (y+size-1)
                lineTo (x+size-1) (y+size-1)
                lineTo (x+size-1) y
                stroke
        showCell :: Int -> Cell -> String
        showCell b m = case (b, m) of
                        (_, Masked) -> "-"
                        (_, Question) -> "?"
                        (_, BaseTypes.Cross) -> "X"
                        (0, Unmasked) -> " "
                        (-1, Unmasked) -> "B"
                        (x, Unmasked) -> show x
