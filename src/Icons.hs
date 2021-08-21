{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Icons where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text (pack)
import System.Directory (doesDirectoryExist, listDirectory, getHomeDirectory)

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
populate listStore iconTheme root = getItems root >>= mconcat . map (addItem listStore iconTheme)

addItem :: Gtk.ListStore -> Gtk.IconTheme -> Item -> IO ()
addItem listStore iconTheme item = do
    iter <- Gtk.listStoreAppend listStore
    value <- Gtk.toGValue . Just . itemPath $ item
    icon <- runMaybeT $ getIcon iconTheme item
    case icon of
        Nothing -> Gtk.listStoreSet listStore iter [0] [value]
        (Just icon') -> Gtk.listStoreSet listStore iter [0, 1] [value, icon']

getIcon :: Gtk.IconTheme -> Item -> MaybeT IO GValue
getIcon iconTheme item = do
    contentType <- Gio.contentTypeGuess (Just . pack $ itemPath item) Nothing
    iconName <- MaybeT $ Gio.contentTypeGetGenericIconName . fst $ contentType
    pixbuf <- Gtk.iconThemeLoadIcon iconTheme iconName 32 []
    liftIO . Gtk.toGValue $ pixbuf

data Item = Item
    { itemPath :: FilePath
    , itemIsDir :: Bool
    } deriving Show

getItems :: FilePath -> IO [Item]
getItems root = do
    path <- listDirectory root
    isDir <- sequence $ doesDirectoryExist . (<>) root <$> path
    return . getZipList $ Item <$> ZipList path <*> ZipList isDir
