module Main where

import qualified GI.Gtk as Gtk
import StatusNotifier.Tray

main :: IO ()
main = do
  Gtk.init Nothing
  tray <- buildTrayWithHost
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.containerAdd window tray
  Gtk.widgetShowAll window
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.main
