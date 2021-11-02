{-# LANGUAGE OverloadedStrings #-}

module Style (
    initStyle
) where

import Control.Monad (when)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

initStyle :: Gtk.ApplicationWindow -> IO ()
initStyle win = do
    setTheme win
    addCSS win

setTheme :: Gtk.ApplicationWindow -> IO ()
setTheme win = do
    screen <- Gtk.windowGetScreen win
    settings <- Gtk.settingsGetForScreen screen
    Gtk.setSettingsGtkApplicationPreferDarkTheme settings True

getConfigDir :: IO FilePath
getConfigDir = do
    fromEnv <- lookupEnv "XDG_CONFIG_HOME"
    case fromEnv of
        Nothing -> (<> "/.config/xanadu") <$> getHomeDirectory
        Just "" -> (<> "/.config/xanadu") <$> getHomeDirectory
        Just path -> return $ path </> "/xanadu"

addCSS :: Gtk.ApplicationWindow -> IO ()
addCSS win = do
    screen <- Gtk.windowGetScreen win
    cssProvider <- Gtk.cssProviderNew
    Gtk.styleContextAddProviderForScreen screen cssProvider 800

    -- Default CSS
    Gtk.cssProviderLoadFromData cssProvider "* { background-color: transparent; }"

    -- Custom CSS
    configDir <- getConfigDir
    let file = configDir </> "style.css"
    hasFile <- doesFileExist file
    when hasFile $ Gtk.cssProviderLoadFromFile cssProvider =<< Gio.fileNewForPath file
