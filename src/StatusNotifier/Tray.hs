module StatusNotifier.Tray where

import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader
import           DBus.Client
import           Data.ByteString
import           Data.ByteString.Unsafe
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import           Foreign.Ptr
import           GI.DbusmenuGtk3.Objects.Menu
import           GI.GLib.Structs.Bytes
import           GI.Gdk.Objects.Screen
import           GI.GdkPixbuf.Callbacks
import           GI.GdkPixbuf.Enums
import           GI.GdkPixbuf.Objects.Pixbuf
import           GI.GdkPixbuf.Structs.Pixdata
import qualified GI.Gtk as Gtk
import           GI.Gtk.Flags
import           GI.Gtk.Objects.IconTheme
import qualified StatusNotifier.Host.Service as H
import           Text.Printf

themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

getThemeWithDefaultFallbacks :: String -> IO IconTheme
getThemeWithDefaultFallbacks themePath = do
  themeForIcon <- iconThemeNew
  defaultTheme <- iconThemeGetDefault
  runMaybeT $ do
    screen <- MaybeT screenGetDefault
    lift $ iconThemeSetScreen themeForIcon screen

  filePaths <- iconThemeGetSearchPath defaultTheme
  iconThemeAppendSearchPath themeForIcon themePath
  mapM_ (iconThemeAppendSearchPath themeForIcon) filePaths

  return defaultTheme

getIconPixbufByName :: IsIconTheme it =>  Int32 -> T.Text -> it -> IO (Maybe Pixbuf)
getIconPixbufByName size name themeForIcon = do
  let panelName = T.pack $ printf "%s-panel" name
  hasPanelIcon <- iconThemeHasIcon themeForIcon panelName
  let targetName = if hasPanelIcon then panelName else name
  iconThemeLoadIcon themeForIcon targetName size themeLoadFlags

getIconPixbufFromByteString :: ByteString -> Int32 -> Int32 -> IO Pixbuf
getIconPixbufFromByteString byteString width height = do
  bytes <- bytesNew $ Just byteString
  let bytesPerPixel = 4
      rowStride = width * bytesPerPixel
      sampleBits = 8
  pixbufNewFromBytes bytes ColorspaceRgb True sampleBits width height rowStride
