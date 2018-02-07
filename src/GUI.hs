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
       windowDefaultWidth := 350,
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
    widgetSetSizeRequest frame 350 350

    -- end mainbox
    containerAdd window mainbox
    widgetShowAll window

    startGame canvas board buttonS
    _ <- widgetGetDrawWindow canvas
    mainGUI

startGame :: DrawingArea -> Board -> Button -> IO (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea)
startGame canvas board buttonS = do
    mfix $ \cids -> defineCallbacks (Context canvas 30 board) buttonS cids

defineCallbacks :: Context -> Button -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> IO (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea)
defineCallbacks context@(Context canvas _ _) buttonS previousCids = do
    liftIO $ putStrLn "redefine callbacks: should disconnect previous event handlers"
    sCid <- onClicked buttonS $ do
        board <- initBoard 10 10 10
        startGame canvas board buttonS
        return ()

    bCid <- on canvas buttonPressEvent $ buttonPressCB context buttonS previousCids
    eCid <- on canvas exposeEvent $ renderBoardCB context previousCids
    return (sCid, bCid, eCid)

disconnectPrevious :: (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> IO()
disconnectPrevious (sCid, bCid, eCid) = do
    signalDisconnect sCid
    signalDisconnect bCid
    signalDisconnect eCid

buttonPressCB :: Context -> Button -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> EventM EButton Bool
buttonPressCB context@(Context canvas size board) buttonS previousCids = do
    dw      <- eventWindow
    (x, y)      <- eventCoordinates
    button <- eventButton
    let (c, r) = (floor $ x / size, floor $ y / size)
    let newBoard@(Board rows) = case button of
                        LeftButton -> (if cellIsMasked board (r, c) then unmaskCell board (r, c) else board)
                        RightButton -> (setTile board (r, c) BaseTypes.Cross)

    liftIO . renderWithDrawable dw $
        mapM_ (renderRowOfBoard size dw) (zip [0..] rows)
    liftIO $ case gameState newBoard of
        Win -> renderWithDrawable dw $ renderWin $ board
        Loss -> renderWithDrawable dw $ renderLoss $ board
        Playing -> do
            disconnectPrevious previousCids
            mfix $ \cids -> defineCallbacks (Context canvas size newBoard) buttonS cids
            return ()

    return True

renderWin :: Board -> Render()
renderWin board = liftIO $ putStrLn "Win!"

renderLoss :: Board -> Render()
renderLoss board = liftIO $ putStrLn "Loss!"

renderBoardCB :: Context -> (ConnectId Button, ConnectId DrawingArea, ConnectId DrawingArea) -> EventM EExpose Bool
renderBoardCB (Context _ size (Board rows)) previousCids = do
    dw      <- eventWindow
    region  <- eventRegion >>= liftIO . regionGetRectangles
    liftIO . renderWithDrawable dw $
        mapM_ (renderRowOfBoard size dw) (zip [0..] rows)

    return True

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

    where
        renderTile :: Double -> Int -> Int -> Cell -> Render()
        renderTile size r c cell =
            let x = (fromIntegral c)*size
                y = (fromIntegral r)*size
            in do
                liftIO $ do drawWindowClearArea dw (floor x) (floor y) (ceiling $ x+size-1) (ceiling $ y+size-1)
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