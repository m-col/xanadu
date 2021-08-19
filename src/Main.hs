{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import Data.Functor (void)
import Data.Maybe
import Data.Void
import GHC.Int (Int32)
import System.Directory (getDirectoryContents)

import Data.GI.Base
import qualified Data.GI.Base.GType as GType
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

main :: IO ()
main = do
    app <- new Gtk.Application
      [ #applicationId := "xanadu"
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
    listStore <- Gtk.listStoreNew [GType.gtypeString]
    view <- new Gtk.IconView
        [ #model := listStore
        , #textColumn := 0
        , #tooltipColumn := 0
        , #pixbufColumn := 1
        ]
    populate listStore "/home/mcol/Desktop"
    #add win view
    #showAll win
    return ()

populate :: Gtk.ListStore -> FilePath -> IO ()
populate listStore root = ls root >>= mconcat . map (addItem listStore)

addItem :: Gtk.ListStore -> FilePath -> IO ()
addItem listStore path = do
    value <- Gtk.toGValue $ Just path
    iter <- Gtk.listStoreAppend listStore
    Gtk.listStoreSet listStore iter [0] [value]

ls :: FilePath -> IO [FilePath]
ls = fmap (filter (\f -> f /= "." && f /= "..")) . getDirectoryContents
