{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Icons where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text (pack)
import System.Directory (getDirectoryContents, getHomeDirectory)

import Data.GI.Base
import qualified Data.GI.Base.GType as GType
import qualified GI.GdkPixbuf as Pixbuf
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

initIcons :: Gtk.ApplicationWindow -> IO Gtk.IconView
initIcons win = do
    pixbufGType <- glibType @Pixbuf.Pixbuf
    listStore <- Gtk.listStoreNew [GType.gtypeString, pixbufGType]
    iconTheme <- Gtk.iconThemeGetForScreen =<< Gtk.windowGetScreen win
    homeDir <- getHomeDirectory
    populate listStore iconTheme $ homeDir <> "/Desktop"
    new Gtk.IconView
        [ #model := listStore
        , #selectionMode := Gtk.SelectionModeMultiple
        , #textColumn := 0
        , #tooltipColumn := 0
        , #pixbufColumn := 1
        ]

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
