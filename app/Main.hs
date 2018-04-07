module Main where

import           Control.Monad
import           DBus.Client
import           Data.Int
import           Data.Ratio
import           Data.Semigroup ((<>))
import qualified GI.Gtk as Gtk
import           Graphics.UI.GIGtkStrut
import           Options.Applicative
import           StatusNotifier.Tray
import           System.Log.Logger
import           System.Posix.Process
import           Text.Printf

positionP :: Parser StrutPosition
positionP =
  flag' TopPos
  (  long "top"
  <> help "Position the bar at the top of the screen."
  ) <|> flag' BottomPos
  (  long "bottom"
  <> help "Position the bar at the bottom of the screen."
  ) <|> flag' LeftPos
  (  long "left"
  <> help "Position the bar on the left side of the screen."
  ) <|> flag' RightPos
  (  long "right"
  <> help "Position the bar on the right side of the screen."
  ) <|> option auto (value TopPos)

alignmentP :: Parser StrutAlignment
alignmentP =
  flag' Beginning
  (  long "beginning"
  <> help "Use beginning alignment."
  ) <|> flag' Center
  (  long "center"
  <> help "Use center alignment."
  ) <|> flag' End
  (  long "end"
  <> help "Use end alignment."
  ) <|> option auto (value Center)

sizeP :: Parser Int32
sizeP =
  option auto
  (  long "size"
  <> help "Set the size of the bar."
  <> value 30
  <> metavar "SIZE"
  )

paddingP :: Parser Int32
paddingP =
  option auto
  (  long "padding"
  <> help "Set the padding of the bar."
  <> value 0
  <> metavar "PADDING"
  )

monitorNumberP :: Parser [Int32]
monitorNumberP = many $
  option auto
  (  long "monitor"
  <> short 'm'
  <> help "Run on the selected monitor."
  <> metavar "MONITOR"
  )

buildWindows :: StrutPosition
             -> StrutAlignment
             -> Int32
             -> Int32
             -> [Int32]
             -> IO ()
buildWindows pos align size padding monitors = do
  client <- connectSession
  logger <- getRootLogger
  pid <- getProcessID
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
        (tray, updateHandler) <- buildTray TrayParams
                            { trayLogger = defaultTrayLogger
                            , trayClient = client
                            , trayOrientation = orientation
                            }
        window <- buildStrutWindow config
        Gtk.containerAdd window tray
        Gtk.widgetShowAll window
        Gtk.onWidgetDestroy window Gtk.mainQuit
        return updateHandler
      runForMonitor monitor =
        buildWithConfig configBase { strutMonitor = Just monitor }
  updateHandlers <-
    if null monitors
    then do
      handler <- buildWithConfig configBase
      return [handler]
    else mapM runForMonitor monitors
  buildHostForHandlers client logger (printf "standalone-%s" $ show pid) updateHandlers

parser :: Parser (IO ())
parser = buildWindows <$> positionP <*> alignmentP <*> sizeP <*> paddingP <*> monitorNumberP

main :: IO ()
main = do
  Gtk.init Nothing
  join $ execParser $ info (parser <**> helper)
               (  fullDesc
               <> progDesc "Run a standalone StatusNotifierItem/AppIndicator tray.")
  Gtk.main
