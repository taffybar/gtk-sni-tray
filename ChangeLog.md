# Changelog for gtk-sni-tray

## 0.1.9.1

- Fix type error with DM.Menu and Gtk.menuPopupAtWidget by adding explicit
  cast to Gtk.Menu. This fixes build failures with newer GI bindings where the
  type hierarchy is not automatically recognized.

## 0.1.9.0

- Use the `gi-gtk3` and `gi-gdk3` build dependencies, which have been
  renamed from `gi-gtk` and `gi-gdk`.

