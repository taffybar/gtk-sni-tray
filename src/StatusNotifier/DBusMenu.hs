{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.DBusMenu
  ( buildMenu
  ) where

import Control.Concurrent (forkIO)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forM_, void, when)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import DBus
import DBus.Client
import Data.GI.Base (unsafeCastTo)
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority(..), logM)
import Text.Printf

import qualified StatusNotifier.DBus.Client.DBusMenu as DM

dbusMenuLogger :: Priority -> String -> IO ()
dbusMenuLogger = logM "StatusNotifier.DBusMenu"

addCssClass :: Gtk.Widget -> T.Text -> IO ()
addCssClass widget cssClass =
  Gtk.widgetGetStyleContext widget >>= (`Gtk.styleContextAddClass` cssClass)

data LayoutNode = LayoutNode
  { lnId :: Int32
  , lnProps :: Map String Variant
  , lnChildren :: [LayoutNode]
  } deriving (Eq, Show)

type LayoutTuple = (Int32, Map String Variant, [Variant])

variantToLayout :: Variant -> Maybe LayoutNode
variantToLayout v = do
  (i, props, kids) <- fromVariant v :: Maybe LayoutTuple
  children <- traverse variantToLayout kids
  pure LayoutNode { lnId = i, lnProps = props, lnChildren = children }

tupleToLayout :: LayoutTuple -> LayoutNode
tupleToLayout (i, props, kids) =
  LayoutNode
    { lnId = i
    , lnProps = props
    , lnChildren = [ n | v <- kids, Just n <- [variantToLayout v] ]
    }

-- | Unwrap an Either MethodError, failing on Left.
unwrapCall :: String -> Either MethodError a -> IO a
unwrapCall label (Left err) = fail $ label <> " failed: " <> show err
unwrapCall _ (Right a) = pure a

aboutToShow :: Client -> BusName -> ObjectPath -> Int32 -> IO Bool
aboutToShow client dest path i =
  either (const False) id <$> DM.aboutToShow client dest path i

getLayout :: Client -> BusName -> ObjectPath -> Int32 -> Int32 -> [String] -> IO (Word32, LayoutNode)
getLayout client dest path parentId depth propNames = do
  (rev, tup) <- unwrapCall "GetLayout" =<<
    DM.getLayout client dest path parentId depth propNames
  pure (rev, tupleToLayout tup)

sendClicked :: Client -> BusName -> ObjectPath -> Int32 -> Word32 -> IO ()
sendClicked client dest path itemId ts = do
  dbusMenuLogger DEBUG $
    printf "sendClicked: id=%d dest=%s path=%s ts=%d"
           itemId (show dest) (show path) ts
  let mc = DM.eventMethodCall
        { methodCallDestination = Just dest
        , methodCallPath = path
        , methodCallBody =
            [ toVariant itemId
            , toVariant ("clicked" :: String)
            , toVariant (toVariant (0 :: Int32))
            , toVariant ts
            ]
        }
  -- Send on a forked thread to avoid blocking GTK; use `call` instead of
  -- `callNoReply` so we can detect service errors.
  void $ forkIO $ catchAny
    (do result <- call client mc
        case result of
          Left err -> dbusMenuLogger WARNING $
            printf "sendClicked: Event error: %s" (show err)
          Right _ -> dbusMenuLogger DEBUG "sendClicked: Event succeeded")
    (\e -> dbusMenuLogger WARNING $
           printf "sendClicked: Event exception: %s" (show e))

getPropS :: String -> LayoutNode -> Maybe String
getPropS key LayoutNode { lnProps = props } =
  Map.lookup key props >>= fromVariant

getPropB :: String -> LayoutNode -> Maybe Bool
getPropB key LayoutNode { lnProps = props } =
  Map.lookup key props >>= fromVariant

getPropI32 :: String -> LayoutNode -> Maybe Int32
getPropI32 key LayoutNode { lnProps = props } =
  Map.lookup key props >>= fromVariant

menuItemType :: LayoutNode -> Maybe String
menuItemType = getPropS "type"

menuItemLabel :: LayoutNode -> String
menuItemLabel n =
  -- libdbusmenu uses "label" with underscores for mnemonics; GTK3 MenuItem
  -- has use-underline support, but defaulting to literal label is fine.
  fromMaybe "" (getPropS "label" n)

menuItemVisible :: LayoutNode -> Bool
menuItemVisible n = fromMaybe True (getPropB "visible" n)

menuItemEnabled :: LayoutNode -> Bool
menuItemEnabled n = fromMaybe True (getPropB "enabled" n)

menuItemToggleType :: LayoutNode -> Maybe String
menuItemToggleType = getPropS "toggle-type"

menuItemToggleState :: LayoutNode -> Maybe Int32
menuItemToggleState = getPropI32 "toggle-state"

populateGtkMenu :: Client -> BusName -> ObjectPath -> Gtk.Menu -> LayoutNode -> IO ()
populateGtkMenu client dest path gtkMenu root = do
  gtkMenuW <- Gtk.toWidget gtkMenu
  addCssClass gtkMenuW "tray-menu"

  -- Clear existing children (for refreshes, e.g. submenus).
  children <- Gtk.containerGetChildren gtkMenu
  forM_ children Gtk.widgetDestroy

  forM_ (lnChildren root) $ \child -> when (menuItemVisible child) $ do
    widget <- buildGtkMenuItem client dest path child
    Gtk.menuShellAppend gtkMenu widget

buildGtkMenuItem :: Client -> BusName -> ObjectPath -> LayoutNode -> IO Gtk.MenuItem
buildGtkMenuItem client dest path node = do
  item <- case menuItemType node of
    Just "separator" -> do
      sep <- Gtk.separatorMenuItemNew
      unsafeCastTo Gtk.MenuItem sep
    _ -> do
      let label = T.pack (menuItemLabel node)
      case menuItemToggleType node of
        Just "checkmark" -> do
          c <- Gtk.checkMenuItemNewWithMnemonic label
          Gtk.checkMenuItemSetActive c (menuItemToggleState node == Just 1)
          unsafeCastTo Gtk.MenuItem c
        Just "radio" -> do
          c <- Gtk.checkMenuItemNewWithMnemonic label
          Gtk.checkMenuItemSetDrawAsRadio c True
          Gtk.checkMenuItemSetActive c (menuItemToggleState node == Just 1)
          unsafeCastTo Gtk.MenuItem c
        _ -> Gtk.menuItemNewWithMnemonic label

  Gtk.widgetSetName item (T.pack ("tray-menu-item-" <> show (lnId node)))
  itemW <- Gtk.toWidget item
  addCssClass itemW "tray-menu-item"

  case menuItemType node of
    Just "separator" -> addCssClass itemW "tray-menu-separator"
    _ -> pure ()

  case menuItemToggleType node of
    Just "checkmark" -> addCssClass itemW "tray-menu-check"
    Just "radio" -> addCssClass itemW "tray-menu-radio"
    _ -> pure ()

  Gtk.widgetSetSensitive item (menuItemEnabled node)

  -- Submenu handling: build children now, and refresh on show via AboutToShow/GetLayout.
  if null (lnChildren node)
    then do
      _ <- Gtk.onMenuItemActivate item $
        catchAny
          (do ts <- Gtk.getCurrentEventTime
              sendClicked client dest path (lnId node) ts)
          (\e -> dbusMenuLogger WARNING $
                 printf "Menu item %d click failed (stale ID?): %s"
                        (lnId node) (show e))
      pure ()
    else do
      addCssClass itemW "tray-menu-item-has-submenu"
      submenu <- Gtk.menuNew
      Gtk.widgetSetName submenu (T.pack ("tray-menu-submenu-" <> show (lnId node)))
      submenuW <- Gtk.toWidget submenu
      addCssClass submenuW "tray-menu-submenu"
      -- Populate with the eagerly-fetched layout so submenus are usable even if
      -- the service doesn't support/require lazy updates.
      populateGtkMenu client dest path submenu node
      let refresh =
            catchAny
              (do -- Allow the service to update the submenu content lazily.
                  _ <- aboutToShow client dest path (lnId node)
                  (_, layout) <- getLayout client dest path (lnId node) 1 []
                  populateGtkMenu client dest path submenu layout
                  Gtk.widgetShowAll submenu)
              (\e -> dbusMenuLogger WARNING $
                     printf "Submenu %d refresh failed (stale ID?): %s"
                            (lnId node) (show e))
      _ <- Gtk.onWidgetShow submenu refresh
      Gtk.menuItemSetSubmenu item (Just submenu)

  pure item

buildMenu :: Client -> BusName -> ObjectPath -> IO Gtk.Menu
buildMenu client dest path = do
  dbusMenuLogger DEBUG $
    printf "buildMenu: dest=%s path=%s" (show dest) (show path)
  _ <- aboutToShow client dest path 0
  (_, layout) <- getLayout client dest path 0 (-1) []
  dbusMenuLogger DEBUG $
    printf "buildMenu: root has %d children" (length (lnChildren layout))
  menu <- Gtk.menuNew
  Gtk.widgetSetName menu "tray-menu-root"
  menuW <- Gtk.toWidget menu
  addCssClass menuW "tray-menu-root"
  populateGtkMenu client dest path menu layout
  pure menu
