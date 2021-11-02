{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Window (
    initWindow
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import GHC.Int (Int32)

import Data.GI.Base
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Enums
import qualified GI.Gtk as Gtk

initWindow :: Gtk.Application -> IO Gtk.ApplicationWindow
initWindow app = do
    win <- new Gtk.ApplicationWindow
        [ #application := app
        , #title := "xanadu"
        , #resizable := False
        ]

    geometry <- runMaybeT getGeometry
    case geometry of
        Nothing -> error "Could not open display."
        Just (width, height) -> Gtk.windowSetDefaultSize win width height

    addCSS win
    Gtk.windowSetTypeHint win Enums.WindowTypeHintDesktop
    return win

getGeometry :: MaybeT IO (Int32, Int32)
getGeometry = do
    display <- MaybeT Gdk.displayGetDefault
    monitor <- MaybeT $ Gdk.displayGetMonitor display 0
    rect <- liftIO $ Gdk.monitorGetGeometry monitor
    width <- Gdk.getRectangleWidth rect
    height <- Gdk.getRectangleHeight rect
    return (width, height)

addCSS :: Gtk.ApplicationWindow -> IO ()
addCSS win = do
    screen <- Gtk.windowGetScreen win
    cssProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData cssProvider "* { background-color: transparent; }"
    Gtk.styleContextAddProviderForScreen screen cssProvider 800
