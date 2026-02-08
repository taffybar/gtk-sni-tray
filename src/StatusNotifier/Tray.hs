{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE CPP #-}
module StatusNotifier.Tray where

import           Control.Concurrent.MVar as MV
import           Control.Exception.Base
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           DBus.Client
import qualified DBus.Internal.Types as DBusTypes
import           Data.Bool (bool)
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Foldable (traverse_)
import qualified Data.GI.Base.ManagedPtr as ManagedPtr
import           Data.GI.Base.GError
import           Data.Int
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import qualified Data.Text as T
import qualified GI.GLib as GLib
import           GI.GLib.Structs.Bytes
import qualified GI.Gdk as Gdk
import           GI.Gdk.Enums
import           GI.Gdk.Objects.Screen
import           GI.Gdk.Structs.EventScroll
import           GI.GdkPixbuf.Enums
import           GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           GI.Gtk.Flags
import           GI.Gtk.Objects.IconTheme
import           Graphics.UI.GIGtkStrut
import qualified StatusNotifier.DBusMenu as DBusMenu
import           StatusNotifier.Host.Service
import qualified StatusNotifier.Item.Client as IC
import           System.Directory
import           System.FilePath
import           System.Log.Logger
import           Text.Printf
import           Foreign.Ptr (Ptr)

trayLogger :: Priority -> String -> IO ()
trayLogger = logM "StatusNotifier.Tray"

logItemInfo :: ItemInfo -> String -> IO ()
logItemInfo info message =
  trayLogger INFO $ printf "%s - %s pixmap count: %s" message
         (show $ info { iconPixmaps = []})
         (show $ length $ iconPixmaps info)

getScaledWidthHeight :: Bool -> Int32 -> Int32 -> Int32 -> (Int32, Int32)
getScaledWidthHeight shouldTargetWidth targetSize width height =
  let getRatio :: Int32 -> Rational
      getRatio toScale =
        fromIntegral targetSize / fromIntegral toScale
      getOther :: Int32 -> Int32 -> Int32
      getOther toScale other = floor $ getRatio toScale * fromIntegral other
  in
    if shouldTargetWidth
    then (targetSize, getOther width height)
    else (getOther height width, targetSize)

scalePixbufToSize :: Int32 -> Gtk.Orientation -> Pixbuf -> IO Pixbuf
scalePixbufToSize size orientation pixbuf = do
  width <- pixbufGetWidth pixbuf
  height <- pixbufGetHeight pixbuf
  let warnAndReturnOrig =
        trayLogger WARNING "Unable to scale pixbuf" >> return pixbuf
      targetWidth = case orientation of
                      Gtk.OrientationHorizontal -> False
                      _ -> True
      (scaledWidth, scaledHeight) =
        getScaledWidthHeight targetWidth size width height
  trayLogger DEBUG $
             printf
             "Scaling pb to %s, actualW: %s, actualH: %s, scaledW: %s, scaledH: %s"
             (show size) (show width) (show height)
             (show scaledWidth) (show scaledHeight)

  trayLogger DEBUG $ printf "targetW: %s, targetH: %s"
               (show scaledWidth) (show scaledHeight)
  maybe warnAndReturnOrig return =<<
    pixbufScaleSimple pixbuf scaledWidth scaledHeight InterpTypeBilinear

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

catchGErrorsAsLeft :: IO a -> IO (Either GError a)
catchGErrorsAsLeft action = catch (Right <$> action) (return . Left)

catchGErrorsAsNothing :: IO a -> IO (Maybe a)
catchGErrorsAsNothing action = catchGErrorsAsLeft action >>= rightToJustLogLeft
       where rightToJustLogLeft (Right value) = return $ Just value
             rightToJustLogLeft (Left err) = do
               trayLogger WARNING $ printf "Encountered error: %s" $ show err
               return Nothing

safePixbufNewFromFile :: FilePath -> IO (Maybe Gdk.Pixbuf)
safePixbufNewFromFile =
  handleResult . catchGErrorsAsNothing . Gdk.pixbufNewFromFile
  where
#if MIN_VERSION_gi_gdkpixbuf(2,0,26)
    handleResult = fmap join
#else
    handleResult = id
#endif

getIconPixbufByName :: Int32 -> T.Text -> Maybe String -> IO (Maybe Pixbuf)
getIconPixbufByName size name themePath = do
  trayLogger DEBUG $ printf "Getting Pixbuf from name for %s" name
  let nonEmptyThemePath = themePath >>= (\x -> if x == "" then Nothing else Just x)
  themeForIcon <-
    maybe iconThemeGetDefault getThemeWithDefaultFallbacks nonEmptyThemePath

  let panelName = T.pack $ printf "%s-panel" name
  hasPanelIcon <- iconThemeHasIcon themeForIcon panelName
  hasIcon <- iconThemeHasIcon themeForIcon name

  if hasIcon || hasPanelIcon

  then do
    let targetName = if hasPanelIcon then panelName else name
    trayLogger DEBUG $ printf "Found icon %s in theme" name
    catchAny (iconThemeLoadIcon themeForIcon targetName size themeLoadFlags)
             (const $ pure Nothing)

  else do
    trayLogger DEBUG $ printf "Trying to load icon %s as filepath" name
    -- Try to load the icon as a filepath
    let nameString = T.unpack name
    fileExists <- doesFileExist nameString
    maybeFile <- if fileExists
    then return $ Just nameString
    else fmap join $ sequenceA $ getIconPathFromThemePath nameString <$> themePath
#if MIN_VERSION_gi_gdkpixbuf(2,0,26)
    let handleResult = fmap join . sequenceA
#else
    let handleResult = sequenceA
#endif
    handleResult $ safePixbufNewFromFile <$> maybeFile

getIconPathFromThemePath :: String -> String -> IO (Maybe String)
getIconPathFromThemePath name themePath = if name == "" then return Nothing else do
  trayLogger DEBUG $ printf
    "Trying to load icon %s as filepath with theme path %s"
    name themePath
  pathExists <- doesDirectoryExist themePath
  if pathExists
  then do
    fileNames <- catchAny (listDirectory themePath) (const $ return [])
    trayLogger DEBUG $ printf
      "Found files in theme path %s" (show fileNames)
    return $ (themePath </>) <$> find (isPrefixOf name) fileNames
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
  , contextMenuPath :: Maybe DBusTypes.ObjectPath
  , contextImage :: Gtk.Image
  , contextButton :: Gtk.EventBox
  }

data TrayImageSize = Expand | TrayImageSize Int32

data TrayClickAction = Activate | SecondaryActivate | PopupMenu

data TrayParams = TrayParams
  { trayOrientation :: Gtk.Orientation
  , trayImageSize :: TrayImageSize
  , trayIconExpand :: Bool
  , trayAlignment :: StrutAlignment
  , trayOverlayScale :: Rational
  , trayLeftClickAction :: TrayClickAction
  , trayMiddleClickAction :: TrayClickAction
  , trayRightClickAction :: TrayClickAction
  }

defaultTrayParams :: TrayParams
defaultTrayParams = TrayParams
  { trayOrientation = Gtk.OrientationHorizontal
  , trayImageSize = Expand
  , trayIconExpand = False
  , trayAlignment = End
  , trayOverlayScale = 3 % 5
  , trayLeftClickAction = Activate
  , trayMiddleClickAction = SecondaryActivate
  , trayRightClickAction = PopupMenu
  }

buildTray :: Host -> Client -> TrayParams -> IO Gtk.Box
buildTray Host
            { itemInfoMap = getInfoMap
            , addUpdateHandler = addUHandler
            , removeUpdateHandler = removeUHandler
            }
          client
          TrayParams { trayOrientation = orientation
                     , trayImageSize = imageSize
                     , trayIconExpand = shouldExpand
                     , trayAlignment = alignment
                     , trayOverlayScale = overlayScale
                     , trayLeftClickAction = leftClickAction
                     , trayMiddleClickAction = middleClickAction
                     , trayRightClickAction = rightClickAction
                     } = do
  trayLogger INFO "Building tray"

  trayBox <- Gtk.boxNew orientation 0
  Gtk.widgetGetStyleContext trayBox >>=
    flip Gtk.styleContextAddClass "tray-box"
  contextMap <- MV.newMVar Map.empty

  let getContext name = Map.lookup name <$> MV.readMVar contextMap
      showInfo info = show info { iconPixmaps = [] }

      getSize rectangle =
        case orientation of
          Gtk.OrientationHorizontal ->
            Gdk.getRectangleHeight rectangle
          _ ->
            Gdk.getRectangleWidth rectangle

      getInfoAttr fn def name = maybe def fn . Map.lookup name <$> getInfoMap

      getInfo :: ItemInfo -> DBusTypes.BusName -> IO ItemInfo
      getInfo = getInfoAttr id

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
                                        else trayLogger WARNING $
                                             printf "Failed to get pixbuf for %s" $
                                             showInfo info
                                  in handlePixbuf

      getTooltipText ItemInfo { itemToolTip = Just (_, _, titleText, fullText )}
        | titleText == fullText = fullText
        | titleText == "" = fullText
        | fullText == "" = titleText
        | otherwise = printf "%s: %s" titleText fullText
      getTooltipText _ = ""

      setTooltipText widget info =
        Gtk.widgetSetTooltipText widget $ Just $ T.pack $ getTooltipText info

      updateHandler ItemAdded
                    info@ItemInfo { menuPath = pathForMenu
                                  , itemServiceName = serviceName
                                  , itemServicePath = servicePath
          } =
        do
          let serviceNameStr = (coerce serviceName :: String)
              servicePathStr = coerce servicePath :: String
              logText = printf "Adding widget for %s - %s"
                        serviceNameStr servicePathStr

          trayLogger INFO logText

          eventBox <- Gtk.eventBoxNew
          Gtk.widgetAddEvents eventBox [Gdk.EventMaskScrollMask]
          Gtk.widgetGetStyleContext eventBox >>=
            flip Gtk.styleContextAddClass "tray-icon-button"

          image <-
            case imageSize of
              Expand -> do
                image <- Gtk.imageNew
                lastAllocation <- MV.newMVar Nothing

                let setPixbuf allocation =
                      do
                        size <- getSize allocation

                        actualWidth <- Gdk.getRectangleWidth allocation
                        actualHeight <- Gdk.getRectangleHeight allocation

                        requestResize <- MV.modifyMVar lastAllocation $ \previous ->
                          let thisTime = Just (size, actualWidth, actualHeight)
                          in return (thisTime, thisTime /= previous)

                        trayLogger DEBUG $
                                   printf
                                   ("Allocating image size %s, width %s," <>
                                    " height %s, resize %s")
                                   (show size)
                                   (show actualWidth)
                                   (show actualHeight)
                                   (show requestResize)

                        when requestResize $ do
                          trayLogger DEBUG "Requesting resize"
                          pixBuf <- getInfo info serviceName >>=
                                    getScaledPixBufFromInfo size
                          when (isNothing pixBuf) $
                               trayLogger WARNING $
                                          printf "Got null pixbuf for info %s" $
                                          showInfo info
                          Gtk.imageSetFromPixbuf image pixBuf
                          void $ traverse
                                 (\pb -> do
                                    width <- pixbufGetWidth pb
                                    height <- pixbufGetHeight pb
                                    Gtk.widgetSetSizeRequest image width height)
                                 pixBuf
                          void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
                                   Gtk.widgetQueueResize image >> return False)

                _ <- Gtk.onWidgetSizeAllocate image setPixbuf
                return image
              TrayImageSize size -> do
                pixBuf <- getScaledPixBufFromInfo size info
                Gtk.imageNewFromPixbuf pixBuf

          Gtk.widgetGetStyleContext image >>=
             flip Gtk.styleContextAddClass "tray-icon-image"

          Gtk.containerAdd eventBox image
          setTooltipText eventBox info

          let context =
                ItemContext { contextName = serviceName
                            , contextMenuPath = pathForMenu
                            , contextImage = image
                            , contextButton = eventBox
                            }

              popupGtkMenu gtkMenu triggerEvent = do
                Gtk.menuAttachToWidget gtkMenu eventBox Nothing
                _ <- Gtk.onWidgetHide gtkMenu (Gtk.widgetDestroy gtkMenu)
                Gtk.widgetShowAll gtkMenu
                evPtr <- ManagedPtr.unsafeManagedPtrCastPtr triggerEvent :: IO (Ptr Gdk.Event)
                ManagedPtr.withTransient evPtr $ \ev ->
                  Gtk.menuPopupAtPointer gtkMenu (Just ev)

          _ <- Gtk.onWidgetButtonPressEvent eventBox $ \event -> do
            mouseButton <- Gdk.getEventButtonButton event
            x <- round <$> Gdk.getEventButtonXRoot event
            y <- round <$> Gdk.getEventButtonYRoot event
            action <- case mouseButton of
              1 -> bool leftClickAction PopupMenu <$> getInfoAttr
                   itemIsMenu True serviceName
              2 -> return middleClickAction
              _ -> return rightClickAction
            case action of
              Activate -> void $ IC.activate client serviceName servicePath x y
              SecondaryActivate -> void $ IC.secondaryActivate client
                                   serviceName servicePath x y
              PopupMenu -> do
                menuPath' <- getInfoAttr menuPath Nothing serviceName
                traverse_
                  (\p -> catchAny
                    (DBusMenu.buildMenu client serviceName p >>= (`popupGtkMenu` event))
                    (\e -> trayLogger WARNING $ printf "Failed to build menu for %s: %s"
                      (coerce serviceName :: String)
                      (show e)))
                  menuPath'
            return False
          _ <- Gtk.onWidgetScrollEvent eventBox $ \event -> do
            direction <- getEventScrollDirection event
            let direction' = case direction of
                               ScrollDirectionUp -> Just "vertical"
                               ScrollDirectionDown -> Just "vertical"
                               ScrollDirectionLeft -> Just "horizontal"
                               ScrollDirectionRight -> Just "horizontal"
                               _ -> Nothing
                -- deltaX/deltaY are provided only in case of smooth scrolling which
                -- is enabled via additional flag, we don't to enable/handle it
                delta = case direction of
                          ScrollDirectionUp -> -1
                          ScrollDirectionDown -> 1
                          ScrollDirectionLeft -> -1
                          ScrollDirectionRight -> 1
                          _ -> 0
            traverse_ (IC.scroll client serviceName servicePath delta) direction'
            return False

          MV.modifyMVar_ contextMap $ return . Map.insert serviceName context

          Gtk.widgetShowAll eventBox
          let packFn =
                case alignment of
                  End -> Gtk.boxPackEnd
                  _ -> Gtk.boxPackStart

          packFn trayBox eventBox shouldExpand True 0

      updateHandler ItemRemoved ItemInfo { itemServiceName = name }
        = getContext name >>= removeWidget
        where removeWidget Nothing =
                trayLogger WARNING "removeWidget: unrecognized service name."
              removeWidget (Just ItemContext { contextButton = widgetToRemove }) =
                do
                  Gtk.containerRemove trayBox widgetToRemove
                  MV.modifyMVar_ contextMap $ return . Map.delete name

      updateHandler IconUpdated i = updateIconFromInfo i
      updateHandler OverlayIconUpdated i = updateIconFromInfo i

      updateHandler ToolTipUpdated info@ItemInfo { itemServiceName = name } =
        void $ getContext name >>=
             traverse (flip setTooltipText info . contextButton)

      updateHandler _ _ = return ()

      maybeAddOverlayToPixbuf size info pixbuf = do
        _ <- runMaybeT $ do
          let overlayHeight = floor (fromIntegral size * overlayScale)
          overlayPixbuf <-
            MaybeT $ getOverlayPixBufFromInfo overlayHeight info >>=
            traverse (scalePixbufToSize overlayHeight Gtk.OrientationHorizontal)
          lift $ do
            actualOHeight <- getPixbufHeight overlayPixbuf
            actualOWidth <- getPixbufWidth overlayPixbuf
            _mainHeight <- getPixbufHeight pixbuf
            _mainWidth <- getPixbufWidth pixbuf
            pixbufComposite overlayPixbuf pixbuf
              0 0                           -- Top left corner
              actualOWidth actualOHeight    -- Overlay size
              0 0                           -- Offset
              1.0 1.0                       -- Scale
              InterpTypeBilinear            -- InterpType
              255                           -- Source image alpha
        return pixbuf

      getScaledPixBufFromInfo size info =
        getPixBufFromInfo size info >>=
        traverse (scalePixbufToSize size orientation >=>
                  maybeAddOverlayToPixbuf size info)

      getPixBufFromInfo size
                        ItemInfo { iconName = name
                                 , iconThemePath = mpath
                                 , iconPixmaps = pixmaps
                                 } = getPixBufFrom size name mpath pixmaps

      getOverlayPixBufFromInfo size
                               ItemInfo
                                     { overlayIconName = name
                                     , iconThemePath = mpath
                                     , overlayIconPixmaps = pixmaps
                                     } = getPixBufFrom size (fromMaybe "" name)
                               mpath pixmaps

      getPixBufFrom size name mpath pixmaps = do
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
        then getIconPixbufByName size (T.pack name) mpath
        else sequenceA $ getFromPixmaps selectedPixmap

      uiUpdateHandler updateType info =
        void $ Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
             updateHandler updateType info >> return False

  handlerId <- addUHandler uiUpdateHandler
  _ <- Gtk.onWidgetDestroy trayBox $ removeUHandler handlerId
  return trayBox
