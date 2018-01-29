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

    mainGUI

renderBoard :: EventM EExpose Bool
renderBoard = do
    dw      <- eventWindow
    region  <- eventRegion >>= liftIO . regionGetRectangles
    liftIO . renderWithDrawable dw $ do { renderCell 0 0 1 Masked }
    return True


renderCell :: Double -> Double -> Int -> Cell -> Render ()
renderCell = renderCellOfSize 15

renderCellOfSize :: Double -> Double -> Double -> Int -> Cell -> Render ()
renderCellOfSize size r c bomb mask =
    let x = c*size
        y = r*size
    in do
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
