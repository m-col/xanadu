{-# LANGUAGE OverloadedStrings #-}

module Style (
    initStyle
) where

import qualified GI.Gtk as Gtk

initStyle :: Gtk.ApplicationWindow -> IO ()
initStyle win = do
    addCSS win

addCSS :: Gtk.ApplicationWindow -> IO ()
addCSS win = do
    screen <- Gtk.windowGetScreen win
    settings <- Gtk.settingsGetForScreen screen
    Gtk.setSettingsGtkApplicationPreferDarkTheme settings True
    cssProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData cssProvider "* { background-color: transparent; }"
    Gtk.styleContextAddProviderForScreen screen cssProvider 800
