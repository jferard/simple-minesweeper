module GUI where

import BaseTypes
import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo
import Logic
import Control.Monad
import Control.Monad.Trans
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
       windowTitle := "Simple Minesweeper (J. Férard)" ]

    -- mainbox
    mainbox <- vBoxNew False 10

    -- text
    msg <- labelNew (Just "A GTK version is under development.")
    containerAdd mainbox msg

    -- button quit
    buttonQ <- buttonNew
    set buttonQ [ buttonLabel := "Quit" ]
    onClicked buttonQ $ do
        widgetDestroy window

    containerAdd mainbox buttonQ

    canvas <- drawingAreaNew

    -- button start
    buttonS <- buttonNew
    set buttonS [ buttonLabel := "Start" ]

    containerAdd mainbox buttonS

    -- frame
    frame <- frameNew
    containerAdd mainbox frame
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
    containerAdd frame canvas
    widgetSetSizeRequest frame (300+2*2) (300+2*2)

    -- end mainbox
    containerAdd window mainbox
    widgetShowAll window

    startGame Context { canvas = canvas, size = 30, board = board, buttonS = buttonS }
    _ <- widgetGetDrawWindow canvas
    mainGUI

startGame :: Context -> IO (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea)
startGame context = do
    mfix $ \cids -> defineCallbacks context cids

-- Callbacks. We keep a reference on connect ids to disconnect signals before every new call.
defineCallbacks :: Context -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> IO (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea)
defineCallbacks context previousCids = do
    liftIO $ putStrLn "redefine callbacks: should disconnect event handlers before a new call"
    sCid <- (buttonS context) `on` buttonPressEvent $ startCB context previousCids
    bCid <- (canvas context) `on` buttonPressEvent $ buttonPressCB context previousCids
    eCid <- (canvas context) `on` exposeEvent $ renderBoardCB context previousCids
    return (sCid, bCid, eCid)

startCB :: Context -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> EventM EButton Bool
startCB context previousCids = do
    liftIO $ do
        dw <- widgetGetDrawWindow $ canvas context
        disconnectPrevious previousCids
        newBoard <- initBoard 10 10 10
        let newContext = context { board = newBoard }
        renderWithDrawable dw $ renderBoard newContext dw
        startGame newContext
    return True


disconnectPrevious :: (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> IO()
disconnectPrevious (sCid, bCid, eCid) = do
    putStrLn "disconnect event handlers"
    signalDisconnect sCid
    signalDisconnect bCid
    signalDisconnect eCid

buttonPressCB :: Context -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> EventM EButton Bool
buttonPressCB context previousCids = do
    dw      <- eventWindow
    (x, y)      <- eventCoordinates
    button <- eventButton
    let s = size context
    let (c, r) = (floor $ x / s, floor $ y / s)
    let previousBoard = board context
    let newBoard = case button of
                        LeftButton -> (if cellIsMasked previousBoard (r, c) then unmaskCell previousBoard (r, c) else previousBoard)
                        RightButton -> (setTile previousBoard (r, c) BaseTypes.Cross)
    let newContext = context { board = newBoard}

    liftIO $ do
        renderWithDrawable dw $ renderBoard newContext dw
        case gameState newBoard of
            Win -> renderWithDrawable dw $ renderWin newContext dw
            Loss -> renderWithDrawable dw $ renderLoss newContext dw
            Playing -> do
                disconnectPrevious previousCids
                mfix $ \cids -> defineCallbacks newContext cids
                return ()

    return True

renderBoardCB :: Context -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> EventM EExpose Bool
renderBoardCB context previousCids = do
    dw      <- eventWindow
    region  <- eventRegion >>= liftIO . regionGetRectangles
    liftIO . renderWithDrawable dw $ renderBoard context dw
    return True

-- Render
renderBoard :: Context -> DrawWindow -> Render()
renderBoard context dw =
    let
        (Board rows) = board context
        s = size context
    in mapM_ (renderRowOfBoard s dw) (zip [0..] rows)
    where
        renderRowOfBoard :: Double -> DrawWindow -> (Int, [Cell]) -> Render ()
        renderRowOfBoard size dw (r, row) = mapM_ (renderCell size dw r) (zip [0..] row)

        renderCell :: Double -> DrawWindow -> Int -> (Int, Cell) -> Render ()
        renderCell size dw r (c, cell) = do
            renderTile size r c cell
            setSourceRGB 0 0 0
            let x = (fromIntegral c)*size+size*0.30 :: Double
            let y = (fromIntegral r)*size+size*0.75 :: Double
            moveTo x y
            case (bomb cell, tile cell) of
                (b, Unmasked) | b >= 0 -> setSourceRGB (0.2*(fromIntegral b)) (1-0.2*(fromIntegral b)) 0
                _  -> setSourceRGB  0 0 0
            showText $ show cell

        renderTile :: Double -> Int -> Int -> Cell -> Render()
        renderTile size r c cell =
            let x = (fromIntegral c)*size
                y = (fromIntegral r)*size
            in do
                liftIO $ drawWindowClearArea dw (floor x) (floor y) (ceiling $ x+size-1) (ceiling $ y+size-1)
                case (tile cell) of
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

renderWin :: Context -> DrawWindow -> Render()
renderWin _ _ = liftIO $ putStrLn "Win!"

renderLoss :: Context -> DrawWindow -> Render()
renderLoss _ _ = liftIO $ putStrLn "Loss!"

