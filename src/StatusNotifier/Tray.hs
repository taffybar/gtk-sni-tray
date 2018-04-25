{-# LANGUAGE OverloadedLabels #-}
module StatusNotifier.Tray where

import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           DBus.Client
import qualified DBus.Internal.Types as DBusTypes
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Int
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as T
import qualified GI.DbusmenuGtk3.Objects.Menu as DM
import qualified GI.GLib as GLib
import           GI.GLib.Structs.Bytes
import qualified GI.Gdk as Gdk
import           GI.Gdk.Enums
import           GI.Gdk.Objects.Screen
import           GI.GdkPixbuf.Enums
import           GI.GdkPixbuf.Objects.Pixbuf
import qualified GI.Gtk as Gtk
import           GI.Gtk.Flags
import           GI.Gtk.Objects.IconTheme
import           Graphics.UI.GIGtkStrut
import           StatusNotifier.Host.Service
import qualified StatusNotifier.Item.Client as IC
import           System.Directory
import           System.Log.Logger
import           Text.Printf

trayLogger :: Priority -> String -> IO ()
trayLogger = logM "StatusNotifier.Tray"

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

getThemeWithDefaultFallbacks :: String -> IO IconTheme
getThemeWithDefaultFallbacks themePath = do
  themeForIcon <- iconThemeNew
  defaultTheme <- iconThemeGetDefault

  _ <- runMaybeT $ do
    screen <- MaybeT screenGetDefault
    lift $ iconThemeSetScreen themeForIcon screen

  filePaths <- iconThemeGetSearchPath defaultTheme
  iconThemeAppendSearchPath themeForIcon themePath
  mapM_ (iconThemeAppendSearchPath themeForIcon) filePaths

  return themeForIcon

getIconPixbufByName :: IsIconTheme it =>  Int32 -> T.Text -> it -> IO (Maybe Pixbuf)
getIconPixbufByName size name themeForIcon = do
  trayLogger DEBUG "Getting Pixbuf from name"
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
  trayLogger DEBUG "Getting Pixbuf from bytestring"
  bytes <- bytesNew $ Just byteString
  let bytesPerPixel = 4
      rowStride = width * bytesPerPixel
      sampleBits = 8
  pixbufNewFromBytes bytes ColorspaceRgb True sampleBits width height rowStride

data ItemContext = ItemContext
  { contextName :: DBusTypes.BusName
  , contextMenu :: Maybe DM.Menu
  , contextImage :: Gtk.Image
  , contextButton :: Gtk.EventBox
  }

data TrayImageSize = Expand | TrayImageSize Int32

data TrayParams = TrayParams
  { trayHost :: Host
  , trayClient :: Client
  , trayOrientation :: Gtk.Orientation
  , trayImageSize :: TrayImageSize
  , trayIconExpand :: Bool
  , trayAlignment :: StrutAlignment
  }

buildTray :: TrayParams -> IO Gtk.Box
buildTray TrayParams { trayHost = Host
                       { itemInfoMap = getInfoMap
                       , addUpdateHandler = addUHandler
                       , removeUpdateHandler = removeUHandler
                       }
                     , trayClient = client
                     , trayOrientation = orientation
                     , trayImageSize = imageSize
                     , trayIconExpand = shouldExpand
                     , trayAlignment = alignment
                     } = do
  trayLogger INFO "Building tray"

  trayBox <- Gtk.boxNew orientation 0
  contextMap <- MV.newMVar Map.empty

  let getContext name = Map.lookup name <$> MV.readMVar contextMap

      getSize rectangle =
        case orientation of
          Gtk.OrientationHorizontal ->
            Gdk.getRectangleHeight rectangle
          _ ->
            Gdk.getRectangleWidth rectangle

      getInfo def name = fromMaybe def . Map.lookup name <$> getInfoMap

      updateIconFromInfo info@ItemInfo { itemServiceName = name } =
        getContext name >>= updateIcon
        where updateIcon Nothing = updateHandler ItemAdded info
              updateIcon (Just ItemContext { contextImage = image } ) = do
                size <- case imageSize of
                          TrayImageSize size -> return size
                          Expand -> Gtk.widgetGetAllocation image >>= getSize
                getScaledPixBufFromInfo size info >>=
                                  let handlePixbuf mpbuf =
                                        if isJust mpbuf
                                        then Gtk.imageSetFromPixbuf image mpbuf
                                        else updateHandler ItemRemoved info
                                  in handlePixbuf

      updateHandler ItemAdded
                    info@ItemInfo { menuPath = pathForMenu
                                  , itemServiceName = serviceName
                                  , itemServicePath = servicePath
                                  } =
        do
          let serviceNameStr = coerce serviceName
              servicePathStr = coerce servicePath :: String
              serviceMenuPathStr = coerce <$> pathForMenu
              logText = printf "Adding widget for %s - %s"
                        serviceNameStr servicePathStr

          trayLogger INFO logText

          button <- Gtk.eventBoxNew

          image <-
            case imageSize of
              Expand -> do
                image <- Gtk.imageNew
                let setPixbuf rectangle =
                      do
                        size <- getSize rectangle
                        pixBuf <- getInfo info serviceName >>= getScaledPixBufFromInfo size
                        Gtk.imageSetFromPixbuf image pixBuf
                        trayLogger INFO $ printf "Setting size to %s" $ show size

                        allocation <- Gtk.widgetGetAllocation image
                        actualWidth <- Gdk.getRectangleWidth allocation
                        actualHeight <- Gdk.getRectangleHeight allocation
                        when (actualWidth /= size || actualHeight /= size) $ do
                             Gtk.widgetSetSizeRequest image size size
                             trayLogger INFO $ printf "Actual width %s" $ show actualWidth
                             Gtk.containerResizeChildren trayBox
                _ <- Gtk.onWidgetSizeAllocate image setPixbuf
                return image
              TrayImageSize size -> do
                pixBuf <- getScaledPixBufFromInfo size info
                Gtk.imageNewFromPixbuf pixBuf

          Gtk.containerAdd button image

          maybeMenu <- sequenceA $ DM.menuNew (T.pack serviceNameStr) .
                       T.pack <$> serviceMenuPathStr

          let context =
                ItemContext { contextName = serviceName
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

          _ <- Gtk.onWidgetButtonPressEvent button $ const popupItemMenu

          MV.modifyMVar_ contextMap $ return . Map.insert serviceName context

          Gtk.widgetShowAll button
          let packFn =
                case alignment of
                  End -> Gtk.boxPackEnd
                  _ -> Gtk.boxPackStart

          packFn trayBox button shouldExpand True 0

      updateHandler ItemRemoved ItemInfo { itemServiceName = name }
        = getContext name >>= removeWidget
        where removeWidget Nothing =
                trayLogger INFO "Attempt to remove widget with unrecognized service name."
              removeWidget (Just ItemContext { contextButton = widgetToRemove }) =
                do
                  Gtk.containerRemove trayBox widgetToRemove
                  MV.modifyMVar_ contextMap $ return . Map.delete name

      updateHandler IconUpdated i = updateIconFromInfo i

      updateHandler IconNameUpdated i = updateIconFromInfo i

      updateHandler _ _ = return ()

      logItemInfo info message =
        trayLogger INFO $ printf "%s - %s pixmap count: %s" message
               (show $ info { iconPixmaps = []})
               (show $ length $ iconPixmaps info)

      getScaledPixBufFromInfo size info = runMaybeT $ do
        pixBuf <- MaybeT $ getPixBufFromInfo size info
        MaybeT $ pixbufScaleSimple pixBuf size size InterpTypeBilinear

      getPixBufFromInfo size
                        info@ItemInfo { iconName = name
                                      , iconThemePath = mpath
                                      , iconPixmaps = pixmaps
                                      } = do
        logItemInfo info "Getting pixbuf"
        themeForIcon <- maybe iconThemeGetDefault getThemeWithDefaultFallbacks mpath
        let tooSmall (w, h, _) = w < size || h < size
            largeEnough = filter (not . tooSmall) pixmaps
            orderer (w1, h1, _) (w2, h2, _) =
              case comparing id w1 w2 of
                EQ -> comparing id h1 h2
                a -> a
            selectedPixmap =
              if null largeEnough
              then maximumBy orderer pixmaps
              else minimumBy orderer largeEnough
            getFromPixmaps (w, h, p) =
              if BS.length p == 0
              then Nothing
              else Just $ getIconPixbufFromByteString w h p
        if null pixmaps
        then getIconPixbufByName size (T.pack name) themeForIcon
        else sequenceA $ getFromPixmaps selectedPixmap

      uiUpdateHandler updateType info =
        void $ Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
             updateHandler updateType info >> return False

  handlerId <- addUHandler uiUpdateHandler
  _ <- Gtk.onWidgetDestroy trayBox $ removeUHandler handlerId
  return trayBox
