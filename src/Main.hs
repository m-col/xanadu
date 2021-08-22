{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import qualified Icons

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
activateApp app = do
    win <- new Gtk.ApplicationWindow
        [ #application := app
        , #title := "xanadu"
        ]
    iconView <- Icons.initIcons win
    #add win iconView
    #showAll win
    return ()
