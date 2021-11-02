{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Window (
    initWindow
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Enums
import qualified GI.Gtk as Gtk
import qualified GI.GtkLayerShell as LS

initWindow :: Gtk.Application -> IO Gtk.ApplicationWindow
initWindow app = do
    win <- new Gtk.ApplicationWindow
        [ #application := app
        , #title := "xanadu"
        , #resizable := False
        ]

    initWayland win
    runMaybeT $ initX11 win
    addCSS win
    return win

initWayland :: Gtk.ApplicationWindow -> IO ()
initWayland win = do
    LS.initForWindow win
    LS.setLayer win LS.LayerBackground
    LS.setKeyboardMode win LS.KeyboardModeOnDemand
    LS.setAnchor win LS.EdgeTop True
    LS.setAnchor win LS.EdgeBottom True
    LS.setAnchor win LS.EdgeLeft True
    LS.setAnchor win LS.EdgeRight True

initX11 :: Gtk.ApplicationWindow -> MaybeT IO ()
initX11 win = do
    Gtk.windowSetTypeHint win Enums.WindowTypeHintDesktop
    display <- MaybeT Gdk.displayGetDefault
    monitor <- MaybeT $ Gdk.displayGetMonitor display 0
    rect <- liftIO $ Gdk.monitorGetGeometry monitor
    width <- Gdk.getRectangleWidth rect
    height <- Gdk.getRectangleHeight rect
    Gtk.windowSetDefaultSize win width height

addCSS :: Gtk.ApplicationWindow -> IO ()
addCSS win = do
    screen <- Gtk.windowGetScreen win
    settings <- Gtk.settingsGetForScreen screen
    Gtk.setSettingsGtkApplicationPreferDarkTheme settings True
    cssProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData cssProvider "* { background-color: transparent; }"
    Gtk.styleContextAddProviderForScreen screen cssProvider 800
