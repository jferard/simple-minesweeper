module GUI where

import qualified Graphics.UI.Gtk as Gtk

printHello :: IO ()
printHello = do
    Gtk.initGUI
    window <- Gtk.windowNew
    Gtk.onDestroy window Gtk.mainQuit
    Gtk.set window [
       Gtk.containerBorderWidth Gtk.:= 10,
       Gtk.windowDefaultWidth Gtk.:= 350,
       Gtk.windowTitle Gtk.:= "Simple Minesweeper (J. Férard)" ]
    mainbox <- Gtk.vBoxNew False 10
    button <- Gtk.buttonNew
    Gtk.set button [ Gtk.buttonLabel Gtk.:= "Ok" ]
    Gtk.onClicked button $ do
        Gtk.widgetDestroy window

    msg <- Gtk.labelNew (Just "A GTK version is under development.")
    Gtk.containerAdd mainbox msg
    Gtk.containerAdd mainbox button
    Gtk.containerAdd window mainbox
    Gtk.widgetShowAll window
    Gtk.mainGUI
