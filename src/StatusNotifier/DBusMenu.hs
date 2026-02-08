{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.DBusMenu
  ( buildMenu
  ) where

import Control.Exception.Enclosed (catchAny)
import Control.Monad (forM_, when)
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

callMenu
  :: Client
  -> BusName
  -> ObjectPath
  -> MemberName
  -> [Variant]
  -> IO [Variant]
callMenu client dest path member body = do
  let call0 =
        (methodCall path "com.canonical.dbusmenu" member)
          { methodCallDestination = Just dest
          , methodCallBody = body
          }
  reply <- call client call0
  case reply of
    Left err -> fail $ "DBusMenu call failed: " <> show err
    Right ret -> pure (methodReturnBody ret)

aboutToShow :: Client -> BusName -> ObjectPath -> Int32 -> IO Bool
aboutToShow client dest path i = do
  body <- callMenu client dest path "AboutToShow" [toVariant i]
  case body of
    (v : _) -> pure $ fromMaybe False (fromVariant v)
    _ -> pure False

getLayout :: Client -> BusName -> ObjectPath -> Int32 -> Int32 -> [String] -> IO (Word32, LayoutNode)
getLayout client dest path parentId depth propNames = do
  body <- callMenu client dest path "GetLayout"
    [ toVariant parentId
    , toVariant depth
    , toVariant propNames
    ]
  case body of
    (revV : layoutV : _) -> do
      rev <- maybe (fail "GetLayout: bad revision") pure (fromVariant revV)
      node <- maybe (fail "GetLayout: bad layout") pure (variantToLayout layoutV)
      pure (rev, node)
    _ -> fail "GetLayout: unexpected reply body"

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

sendClicked :: Client -> BusName -> ObjectPath -> Int32 -> Word32 -> IO ()
sendClicked client dest path itemId ts = do
  -- "clicked" is the common event for activating menu items in the DBusMenu spec.
  _ <- callMenu client dest path "Event"
    [ toVariant itemId
    , toVariant ("clicked" :: String)
    , toVariant ("" :: String) -- v (data)
    , toVariant ts
    ]
  pure ()

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
  dbusMenuLogger DEBUG "Building DBusMenu Gtk.Menu"
  _ <- aboutToShow client dest path 0
  (_, layout) <- getLayout client dest path 0 (-1) []
  menu <- Gtk.menuNew
  Gtk.widgetSetName menu "tray-menu-root"
  menuW <- Gtk.toWidget menu
  addCssClass menuW "tray-menu-root"
  populateGtkMenu client dest path menu layout
  pure menu
