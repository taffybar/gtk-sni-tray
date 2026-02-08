# Changelog for gtk-sni-tray

## 0.1.10.2

- Fix menu popups not appearing: restore `widgetShowAll` so menu items are
  visible, and use `menuPopupAtPointer` for reliable positioning on both X11
  and Wayland.
- Fix all `-Wall` warnings across library and executable.

## 0.1.10.1

- Fix menu popup positioning on Wayland: use `menuAttachToWidget` instead of
  premature `widgetShowAll` so that `menuPopupAtWidget` can properly anchor the
  popup surface to the tray icon.

## 0.1.10.0

- Replace `libdbusmenu` usage (`gi-dbusmenugtk3`) with a pure Haskell
  implementation of the `com.canonical.dbusmenu` protocol.
- Add CSS style classes/names to tray menu widgets to make them themeable.

## 0.1.9.1

- Fix type error with DM.Menu and Gtk.menuPopupAtWidget by adding explicit
  cast to Gtk.Menu. This fixes build failures with newer GI bindings where the
  type hierarchy is not automatically recognized.

## 0.1.9.0

- Use the `gi-gtk3` and `gi-gdk3` build dependencies, which have been
  renamed from `gi-gtk` and `gi-gdk`.
