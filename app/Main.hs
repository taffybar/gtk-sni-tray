{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           DBus.Client
import           Data.Int
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.UI.GIGtkStrut
import           Options.Applicative
import qualified StatusNotifier.Host.Service as Host
import           StatusNotifier.Tray
import           System.Log.Logger
import           System.Posix.Process
import           Text.Printf

import           Paths_gtk_sni_tray (version)

positionP :: Parser StrutPosition
positionP = fromMaybe TopPos <$> optional
  (   flag' TopPos
  (  long "top"
  <> help "Position the bar at the top of the screen"
  )
  <|> flag' BottomPos
  (  long "bottom"
  <> help "Position the bar at the bottom of the screen"
  )
  <|> flag' LeftPos
  (  long "left"
  <> help "Position the bar on the left side of the screen"
  )
  <|> flag' RightPos
  (  long "right"
  <> help "Position the bar on the right side of the screen"
  ))

alignmentP :: Parser StrutAlignment
alignmentP = fromMaybe Center <$> optional
  (   flag' Beginning
  (  long "beginning"
  <> help "Use beginning alignment"
  )
  <|> flag' Center
  (  long "center"
  <> help "Use center alignment"
  )
  <|> flag' End
  (  long "end"
  <> help "Use end alignment"
  ))

sizeP :: Parser Int32
sizeP =
  option auto
  (  long "size"
  <> short 's'
  <> help "Set the size of the bar"
  <> value 30
  <> metavar "SIZE"
  )

paddingP :: Parser Int32
paddingP =
  option auto
  (  long "padding"
  <> short 'p'
  <> help "Set the padding of the bar"
  <> value 0
  <> metavar "PADDING"
  )

monitorNumberP :: Parser [Int32]
monitorNumberP = many $
  option auto
  (  long "monitor"
  <> short 'm'
  <> help "Display a tray bar on the given monitor"
  <> metavar "MONITOR"
  )

logP :: Parser Priority
logP =
  option auto
  (  long "log-level"
  <> short 'l'
  <> help "Set the log level"
  <> metavar "LEVEL"
  <> value WARNING
  )

colorP :: Parser String
colorP =
  strOption
  (  long "color"
  <> short 'c'
  <> help "Set the background color of the tray; See https://developer.gnome.org/gdk3/stable/gdk3-RGBA-Colors.html#gdk-rgba-parse for acceptable values"
  <> metavar "COLOR"
  <> value "#000000"
  )

expandP :: Parser Bool
expandP =
  switch
  (  long "expand"
  <> help "Let icons expand into the space allocated to the tray"
  <> short 'e'
  )

startWatcherP :: Parser Bool
startWatcherP =
  switch
  (  long "watcher"
  <> help "Start a Watcher to handle SNI registration if one does not exist"
  <> short 'w'
  )

barLengthP :: Parser Rational
barLengthP =
  option auto
  (  long "length"
  <> help "Set the proportion of the screen that the tray bar should occupy -- values are parsed as haskell rationals (e.g. 1 % 2)"
  <> value 1
  )

getColor colorString = do
  rgba <- Gdk.newZeroRGBA
  colorParsed <- Gdk.rGBAParse rgba (T.pack colorString)
  unless colorParsed $ do
    logM "StatusNotifier.Tray" WARNING "Failed to parse provided color"
    void $ Gdk.rGBAParse rgba "#000000"
  return rgba

buildWindows :: StrutPosition
             -> StrutAlignment
             -> Int32
             -> Int32
             -> [Int32]
             -> Priority
             -> String
             -> Bool
             -> Bool
             -> Rational
             -> IO ()
buildWindows pos align size padding monitors priority colorString expand startWatcher length = do
  Gtk.init Nothing
  logger <- getLogger "StatusNotifier"
  saveGlobalLogger $ setLevel priority logger
  client <- connectSession
  logger <- getRootLogger
  pid <- getProcessID
  -- Okay to use a forced pattern here because we want to die if this fails anyway
  Just host <- Host.build Host.defaultParams
    { Host.dbusClient = Just client
    , Host.uniqueIdentifier = printf "standalone-%s" $ show pid
    , Host.startWatcher = startWatcher
    }
  let c1 = defaultStrutConfig
           { strutPosition = pos
           , strutAlignment = align
           , strutXPadding = padding
           , strutYPadding = padding
           }
      defaultRatio = ScreenRatio length
      configBase = case pos of
             TopPos -> c1
                       { strutHeight = ExactSize size
                       , strutWidth = defaultRatio
                       }
             BottomPos -> c1
                          { strutHeight = ExactSize size
                          , strutWidth = defaultRatio
                          }
             RightPos -> c1
                         { strutHeight = defaultRatio
                         , strutWidth = ExactSize size
                         }
             LeftPos -> c1
                        { strutHeight = defaultRatio
                        , strutWidth = ExactSize size
                        }
      buildWithConfig config = do
        let orientation =
              case strutPosition config of
                TopPos -> Gtk.OrientationHorizontal
                BottomPos -> Gtk.OrientationHorizontal
                _ -> Gtk.OrientationVertical
        tray <- buildTray TrayParams
                    { trayClient = client
                    , trayOrientation = orientation
                    , trayHost = host
                    , trayImageSize = Expand
                    , trayIconExpand = expand
                    , trayAlignment = align
                    }
        window <- Gtk.windowNew Gtk.WindowTypeToplevel
        setupStrutWindow config window
        (Just <$> getColor colorString) >>=
             Gtk.widgetOverrideBackgroundColor window [Gtk.StateFlagsNormal]
        Gtk.containerAdd window tray
        Gtk.widgetShowAll window
      runForMonitor monitor =
        buildWithConfig configBase { strutMonitor = Just monitor }
  if null monitors
  then buildWithConfig configBase
  else mapM_ runForMonitor monitors
  Gtk.main

parser :: Parser (IO ())
parser =
  buildWindows <$> positionP <*> alignmentP <*> sizeP <*> paddingP <*>
  monitorNumberP <*> logP <*> colorP <*> expandP <*> startWatcherP <*>
  barLengthP

versionOption :: Parser (a -> a)
versionOption = infoOption
                (printf "gtk-sni-tray-standalone %s" $ showVersion version)
                (  long "version"
                <> help "Show the version number of gtk-sni-tray"
                )

main :: IO ()
main =
  join $ execParser $ info (helper <*> versionOption <*> parser)
               (  fullDesc
               <> progDesc "Run a standalone StatusNotifierItem/AppIndicator tray"
               )
