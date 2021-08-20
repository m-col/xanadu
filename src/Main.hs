{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import Data.Functor (void)
import Data.Maybe
import Data.Text (pack)
import Data.Void
import GHC.Int (Int32)
import System.Directory (getDirectoryContents, getHomeDirectory)

import Data.GI.Base
import qualified Data.GI.Base.GType as GType
import qualified GI.GdkPixbuf as Pixbuf
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
    pixbufGType <- glibType @Pixbuf.Pixbuf
    listStore <- Gtk.listStoreNew [GType.gtypeString, pixbufGType]
    view <- new Gtk.IconView
        [ #model := listStore
        , #selectionMode := Gtk.SelectionModeMultiple
        , #textColumn := 0
        , #tooltipColumn := 0
        , #pixbufColumn := 1
        ]
    screen <- Gtk.windowGetScreen win
    iconTheme <- Gtk.iconThemeGetForScreen screen
    homeDir <- getHomeDirectory
    populate listStore iconTheme $ homeDir <> "/Desktop"
    #add win view
    #showAll win
    return ()

populate :: Gtk.ListStore -> Gtk.IconTheme -> FilePath -> IO ()
populate listStore iconTheme root = ls root >>= mconcat . map (addItem listStore iconTheme)

-- The Gtk is working, but that's some ugly haskell
addItem :: Gtk.ListStore -> Gtk.IconTheme -> FilePath -> IO ()
addItem listStore iconTheme path = do
    iter <- Gtk.listStoreAppend listStore
    value <- Gtk.toGValue $ Just path
    contentType <- Gio.contentTypeGuess (Just . pack $ path) Nothing
    iconName <- Gio.contentTypeGetGenericIconName $ fst contentType
    case iconName of
        Nothing -> Gtk.listStoreSet listStore iter [0] [value]
        (Just iconName') -> do
            maybePixbuf <- Gtk.iconThemeLoadIcon iconTheme iconName' 32 []
            case maybePixbuf of
                Nothing -> Gtk.listStoreSet listStore iter [0] [value]
                pixbuf -> do
                    pixbuf' <- Gtk.toGValue pixbuf
                    Gtk.listStoreSet listStore iter [0, 1] [value, pixbuf']

ls :: FilePath -> IO [FilePath]
ls = fmap (filter (\f -> f /= "." && f /= "..")) . getDirectoryContents
