{-# LANGUAGE OverloadedLabels #-}
module StatusNotifier.Tray where

import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader
import           DBus.Client
import qualified DBus.Internal.Types as DBusTypes
import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe
import           Data.Coerce
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import           Foreign.Ptr
import qualified GI.DbusmenuGtk3.Objects.Menu as DM
import qualified GI.GLib as GLib
import           GI.GLib.Structs.Bytes
import qualified GI.Gdk as Gdk
import           GI.Gdk.Enums
import           GI.Gdk.Objects.Screen
import           GI.GdkPixbuf.Callbacks
import           GI.GdkPixbuf.Enums
import           GI.GdkPixbuf.Objects.Pixbuf
import           GI.GdkPixbuf.Structs.Pixdata
import qualified GI.Gtk as Gtk
import           GI.Gtk.Flags
import qualified GI.Gtk.Objects.Box as Gtk
import qualified GI.Gtk.Objects.HBox as Gtk
import           GI.Gtk.Objects.IconTheme
import           StatusNotifier.Host.Service
import qualified StatusNotifier.Item.Client as IC
import           StatusNotifier.Util
import           System.Directory
import           System.Log.Logger
import           System.Posix.Process
import           Text.Printf

defaultTrayLogger = makeDefaultLogger "StatusNotifier.Tray"

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

  return themeForIcon

getIconPixbufByName :: IsIconTheme it =>  Int32 -> T.Text -> it -> IO (Maybe Pixbuf)
getIconPixbufByName size name themeForIcon = do
  let panelName = T.pack $ printf "%s-panel" name
  hasPanelIcon <- iconThemeHasIcon themeForIcon panelName
  hasIcon <- iconThemeHasIcon themeForIcon name
  if hasIcon || hasPanelIcon
  then do
    let targetName = if hasPanelIcon then panelName else name
    iconThemeLoadIcon themeForIcon targetName size themeLoadFlags
  else do
    -- Try to load the icon as a filepath
    let nameString = T.unpack name
    fileExists <- doesFileExist nameString
    if fileExists
    then Just <$> pixbufNewFromFile name
    else return Nothing

getIconPixbufFromByteString :: Int32 -> Int32 -> BS.ByteString -> IO Pixbuf
getIconPixbufFromByteString width height byteString = do
  bytes <- bytesNew $ Just byteString
  let bytesPerPixel = 4
      rowStride = width * bytesPerPixel
      sampleBits = 8
  pixbufNewFromBytes bytes ColorspaceRgb True sampleBits width height rowStride

data ItemContext = ItemContext
  { contextInfo :: ItemInfo
  , contextMenu :: Maybe DM.Menu
  , contextImage :: Gtk.Image
  , contextButton :: Gtk.EventBox
  }

data TrayParams = TrayParams
  { trayLogger :: Logger
  , trayClient :: Client
  }

buildTrayWithHost :: IO Gtk.Widget
buildTrayWithHost = do
  client <- connectSession
  logger <- getRootLogger
  pid <- getProcessID
  (tray, updateHandler) <- buildTray
                           TrayParams
                           { trayLogger = defaultTrayLogger
                           , trayClient = client
                           }
  _ <- join $ build defaultParams
       { uniqueIdentifier = printf "standalone-%s" $ show pid
       , handleUpdate = updateHandler
       , dbusClient = Just client
       }
  widget <- Gtk.toWidget tray
  return widget

buildTray :: TrayParams -> IO (Gtk.Box, UpdateType -> ItemInfo -> IO ())
buildTray TrayParams { trayLogger = logger
                     , trayClient = client
                     } = do
  logL logger INFO "Building tray"

  trayBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
  widgetMap <- MV.newMVar Map.empty

  let getContext name = Map.lookup name <$> MV.readMVar widgetMap

      updateHandler ItemAdded
                    info@ItemInfo { menuPath = pathForMenu
                                  , itemServiceName = serviceName
                                  , itemServicePath = servicePath
                                  } =
        do
          let serviceNameStr = coerce serviceName
              servicePathStr = coerce servicePath :: String
              serviceMenuPathStr = coerce <$> pathForMenu
              logText = printf "Adding widget for %s - %s."
                        serviceNameStr servicePathStr

          logL logger INFO logText

          pixBuf <- getPixBufFromInfo info
          image <- Gtk.imageNewFromPixbuf pixBuf
          button <- Gtk.eventBoxNew
          maybeMenu <- DM.menuNew (T.pack serviceNameStr) . T.pack <<$>> serviceMenuPathStr

          Gtk.containerAdd button image
          Gtk.widgetShowAll button
          Gtk.boxPackStart trayBox button True True 0

          let context =
                ItemContext { contextInfo = info
                            , contextMenu = maybeMenu
                            , contextImage = image
                            , contextButton = button
                            }
              popupItemForMenu menu =
                Gtk.menuPopupAtWidget menu image
                   GravitySouthWest GravityNorthWest Nothing
              popupItemMenu =
                maybe activateItem popupItemForMenu maybeMenu >> return False
              activateItem = void $ IC.activate client serviceName servicePath 0 0

          Gtk.onWidgetButtonPressEvent button $ const popupItemMenu

          MV.modifyMVar_ widgetMap $ return . Map.insert serviceName context

      updateHandler ItemRemoved ItemInfo { itemServiceName = name }
        = getContext name >>= removeWidget
        where removeWidget Nothing =
                logL logger INFO "Attempt to remove widget with unrecognized service name."
              removeWidget (Just ItemContext { contextButton = widgetToRemove }) =
                do
                  Gtk.containerRemove trayBox widgetToRemove
                  MV.modifyMVar_ widgetMap $ return . Map.delete name

      updateHandler IconUpdated info@ItemInfo { itemServiceName = name } =
        getContext name >>= updateIcon
        where updateIcon Nothing = updateHandler ItemAdded info
              updateIcon (Just ItemContext { contextImage = image } ) =
                getPixBufFromInfo info >>=
                                  let handlePixbuf mpbuf =
                                        if isJust mpbuf
                                        then Gtk.imageSetFromPixbuf image mpbuf
                                        else updateHandler ItemRemoved info
                                  in handlePixbuf

      updateHandler _ _ = return ()

      logItemInfo info message =
        logL logger INFO $ printf "%s - %s pixmap count: %s" message
               (show $ info { iconPixmaps = []})
               (show $ length $ iconPixmaps info)

      getPixBufFromInfo info@ItemInfo { iconName = name
                                      , iconThemePath = mpath
                                      , iconPixmaps = pixmaps
                                      } = do
        logItemInfo info "Getting pixbuf"
        themeForIcon <- maybe iconThemeGetDefault getThemeWithDefaultFallbacks mpath
        -- TODO: Make icon size configurable
        mpixBuf <- getIconPixbufByName 30 (T.pack name) themeForIcon
        let getFromPixmaps (w, h, p) =
              if BS.length p == 0
              then Nothing
              else Just $ getIconPixbufFromByteString w h p
        if length pixmaps == 0
        then return mpixBuf
        else sequenceA $ listToMaybe pixmaps >>= getFromPixmaps

      uiUpdateHandler updateType info =
        void $ Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
             updateHandler updateType info >> return False

  return (trayBox, uiUpdateHandler)

