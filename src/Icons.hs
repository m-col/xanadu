{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Icons where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text (pack)
import System.Directory (doesDirectoryExist, listDirectory, getHomeDirectory)
import System.Environment (lookupEnv)

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
    root <- getRoot
    populate listStore iconTheme root
    iconView <- new Gtk.IconView
        [ #model := listStore
        , #selectionMode := Gtk.SelectionModeMultiple
        , #textColumn := 0
        , #tooltipColumn := 0
        , #pixbufColumn := 1
        ]
    on iconView #itemActivated $ onItemActivated listStore
    return iconView

-- Root folder is $(XDG_DESKTOP_DIR:-$HOME/Desktop)
getRoot :: IO FilePath
getRoot = do
    fromEnv <- lookupEnv "XDG_DESKTOP_DIR"
    case fromEnv of
        Nothing -> liftM (flip (<>) "/Desktop") getHomeDirectory
        Just "" -> liftM (flip (<>) "/Desktop") getHomeDirectory
        Just path -> return path

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

onItemActivated :: Gtk.ListStore -> Gtk.TreePath -> IO ()
onItemActivated listStore treePath = do
    (_, iter) <- Gtk.treeModelGetIter listStore treePath
    gvalue <- Gtk.treeModelGetValue listStore iter 0
    maybeValue <- (Gtk.fromGValue gvalue :: IO (Maybe String))
    case maybeValue of
        Nothing -> return ()
        Just value -> putStrLn $ show value
