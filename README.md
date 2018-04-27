gtk-sni-tray
===============

gtk-sni-tray provides a [StatusNotifierHost](https://www.freedesktop.org/wiki/Specifications/StatusNotifierItem/StatusNotifierHost/) widget written using the gtk+3 bindings for haskell provided by [gi-gtk](https://hackage.haskell.org/package/gi-gtk). It also provides a simple standalone executable, `gtk-sni-tray-standalone`, that is configured with command line arguments. This executable will run the aforementioned widget by itself in a strut window, on each monitor for each it is requested.

taffybar
----------
It is generally recommeneded that you use this widget through [taffybar](https://github.com/travitch/taffybar) with [this module](https://github.com/travitch/taffybar/blob/master/src/System/Taffybar/Widget/SNITray.hs), which will allow you to combine it with other useful widgets, and will give more flexibility in configuration.

StatusNotifierWatcher
--------------------------
By default, it is assumed that you are running an isolated StatusNotifierWatcher daemon. [status-notifier-item](https://github.com/IvanMalison/status-notifier-item) provides a StatusNotifierWatcher executable that you can use for this purpose. If you get an error like

```
MethodError {methodErrorName = ErrorName "org.freedesktop.DBus.Error.ServiceUnknown", methodErrorSerial = Serial 7, methodErrorSender = Just (BusName "org.freedesktop.DBus"), methodErrorDestination = Just (BusName ":1.549"), methodErrorBody = [Variant "The name org.kde.StatusNotifierWatcher was not provided by any .service files"]}
```

when you start `gtk-sni-tray-standalone` it is probably because you have not started a StatusNotifierWatcher on your system. You can solve this problem by passing the `--watcher` flag to `gtk-sni-tray-standalone`, but this is not recommeneded, because many SNI processes do not monitor for new watcher processes, and so may not immediately register when this new watcher is started.

Installation
---------------

Both [`stack`](https://www.haskell.org/cabal/download.html) and [`cabal`](https://www.haskell.org/cabal/download.html) can be used to install gtk-sni-tray.

