{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           DBus.Client
import           Data.Int
import           Data.Maybe
import           Data.Ratio
import           Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import           Graphics.UI.GIGtkStrut
import           Options.Applicative
import qualified StatusNotifier.Host.Service as Host
import           StatusNotifier.Tray
import           System.Log.Logger
import           System.Posix.Process
import           Text.Printf

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
  <> help "Run on the selected monitor"
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
  <> help "Set the background color of the tray"
  <> metavar "COLOR"
  <> value "000000"
  )

expandP :: Parser Bool
expandP =
  switch
  (  long "expand"
  <> help "Whether to let icons expand into the space allocated to the tray"
  <> short 'e'
  )

getColor colorString = do
  rgba <- Gdk.newZeroRGBA
  colorParsed <- Gdk.rGBAParse rgba (T.pack colorString)
  unless colorParsed $ do
    logM "StatusNotifier.Tray" WARNING "Failed to parse provided color"
    void $ Gdk.rGBAParse rgba "000000"
  return rgba

buildWindows :: StrutPosition
             -> StrutAlignment
             -> Int32
             -> Int32
             -> [Int32]
             -> Priority
             -> String
             -> Bool
             -> IO ()
buildWindows pos align size padding monitors priority colorString expand = do
  Gtk.init Nothing
  logger <- getLogger "StatusNotifier"
  saveGlobalLogger $ setLevel priority logger
  client <- connectSession
  logger <- getRootLogger
  pid <- getProcessID
  host <- Host.build Host.defaultParams
    { Host.dbusClient = Just client
    , Host.uniqueIdentifier = printf "standalone-%s" $ show pid
    }
  let c1 = defaultStrutConfig
           { strutPosition = pos
           , strutAlignment = align
           , strutXPadding = padding
           , strutYPadding = padding
           }
      defaultRatio = ScreenRatio (4 % 5)
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
parser = buildWindows <$> positionP <*> alignmentP <*> sizeP <*> paddingP <*>
         monitorNumberP <*> logP <*> colorP <*> expandP

main :: IO ()
main = do
  -- TODO: start watcher if it doesn't exist
  join $ execParser $ info (parser <**> helper)
               (  fullDesc
               <> progDesc "Run a standalone StatusNotifierItem/AppIndicator tray")
