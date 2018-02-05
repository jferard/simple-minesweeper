module GUI where

import BaseTypes
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Logic
import Control.Monad
import Control.Monad.Fix

data GUI = GUI

initGame :: GUI -> Double -> Board -> IO()
initGame gui 25 board = do
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

    defineCallbacks $ Context canvas 30 board
    start


defineCallbacks :: Context -> IO()
defineCallbacks context@(Context canvas _ _) = do
    mfix $ \cid -> on canvas buttonPressEvent $ buttonPressCB context cid
    on canvas exposeEvent $ renderBoardCB context
    return ()

start :: IO()
start = mainGUI

buttonPressCB :: Context -> ConnectId DrawingArea -> EventM EButton Bool
buttonPressCB context@(Context canvas size board) cid = do
    dw      <- eventWindow
    (x, y)      <- eventCoordinates
    button <- eventButton
    let (c, r) = (floor $ x / size, floor $ y / size)
    let newBoard@(Board bombGrid maskGrid) = case button of
                        LeftButton -> (if cellIsMasked board (r, c) then unmaskCell board (r, c) else setCell board (r, c) Masked)
                        RightButton -> (setCell board (r, c) BaseTypes.Cross)

    liftIO . signalDisconnect $ cid
    liftIO . defineCallbacks $ Context canvas size newBoard
    liftIO . renderWithDrawable dw $
        mapM_ (renderRowOfBoard size dw) (zip3 [0..] bombGrid maskGrid)

    return True

renderBoardCB :: Context -> EventM EExpose Bool
renderBoardCB (Context _ size (Board bombGrid maskGrid)) = do
    dw      <- eventWindow
    region  <- eventRegion >>= liftIO . regionGetRectangles
    liftIO . renderWithDrawable dw $
        mapM_ (renderRowOfBoard size dw) (zip3 [0..] bombGrid maskGrid)

    return True

renderRowOfBoard :: Double -> DrawWindow -> (Int, [Int], [Cell]) -> Render ()
renderRowOfBoard size dw (r, bombRow, maskRow) = mapM_ (renderCell size dw r) (zip3 [0..] bombRow maskRow)

renderCell :: Double -> DrawWindow -> Int -> (Int, Int, Cell) -> Render ()
renderCell size dw r (c, bomb, mask) = do
    renderTile size r c mask
    setSourceRGB 0 0 0
    let x = (fromIntegral c)*size+size*0.30 :: Double
    let y = (fromIntegral r)*size+size*0.75 :: Double
    moveTo x y
    case (bomb, mask) of
        (b, Unmasked) | b >= 0 -> setSourceRGB (0.2*(fromIntegral b)) (1-0.2*(fromIntegral b)) 0
        _  -> setSourceRGB  0 0 0
    showText $ showCell bomb mask

    where
        renderTile :: Double -> Int -> Int -> Cell -> Render()
        renderTile size r c mask =
            let x = (fromIntegral c)*size
                y = (fromIntegral r)*size
            in do
                liftIO $ do drawWindowClearArea dw (floor x) (floor y) (ceiling $ x+size-1) (ceiling $ y+size-1)
                case mask of
                    Unmasked -> return()
                    _ -> do
                                setSourceRGB 0.7 0.7 0.7
                                Graphics.Rendering.Cairo.rectangle x y (x+size) (y+size)
                                Graphics.Rendering.Cairo.fill

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
                        (_, Masked) -> ""
                        (_, Question) -> "?"
                        (_, BaseTypes.Cross) -> "X"
                        (0, Unmasked) -> " "
                        (-1, Unmasked) -> "B"
                        (x, Unmasked) -> show x
