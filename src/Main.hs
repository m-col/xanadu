{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Icons (initIcons)
import Style (initStyle)
import Window (initWindow)

import Control.Monad ((<=<))
import Data.GI.Base
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
    app <- new Gtk.Application
      [ #applicationId := "xyz.mcol.xanadu"
      , #flags := [ Gio.ApplicationFlagsFlagsNone ]
      ]
    on app #activate $ activateApp app
    Gio.applicationRun app Nothing
    return ()

activateApp :: Gtk.Application -> IO ()
activateApp =
    mconcat . sequenceA [initStyle, initIcons, Gtk.widgetShowAll] <=< initWindow
