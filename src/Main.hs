{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
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

addItem :: Gtk.ListStore -> Gtk.IconTheme -> FilePath -> IO ()
addItem listStore iconTheme path = do
    iter <- Gtk.listStoreAppend listStore
    value <- Gtk.toGValue $ Just path
    icon <- runMaybeT $ getIcon iconTheme path
    case icon of
        Nothing -> Gtk.listStoreSet listStore iter [0] [value]
        (Just icon') -> Gtk.listStoreSet listStore iter [0, 1] [value, icon']

getIcon :: Gtk.IconTheme -> FilePath -> MaybeT IO GValue
getIcon iconTheme path = do
    contentType <- Gio.contentTypeGuess (Just . pack $ path) Nothing
    iconName <- MaybeT $ Gio.contentTypeGetGenericIconName . fst $ contentType
    pixbuf <- Gtk.iconThemeLoadIcon iconTheme iconName 32 []
    liftIO . Gtk.toGValue $ pixbuf

ls :: FilePath -> IO [FilePath]
ls = fmap (filter (\f -> f /= "." && f /= "..")) . getDirectoryContents
