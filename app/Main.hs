module Main where

import           Control.Monad
import           Data.Int
import           Data.Ratio
import           Data.Semigroup ((<>))
import qualified GI.Gtk as Gtk
import           Graphics.UI.GIGtkStrut
import           Options.Applicative
import           StatusNotifier.Tray

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

buildWindows ::
  Foldable t =>
  StrutPosition
  -> StrutAlignment -> Int32 -> Int32 -> t Int32 -> IO ()
buildWindows pos align size padding monitors = do
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
      runForMonitor monitor =
        buildWithConfig configBase { strutMonitor = Just monitor }
  if null monitors
  then buildWithConfig configBase
  else mapM_ runForMonitor monitors

parser :: Parser (IO ())
parser = buildWindows <$> positionP <*> alignmentP <*> sizeP <*> paddingP <*> monitorNumberP

buildWithConfig :: StrutConfig -> IO ()
buildWithConfig config = do
  let orientation =
        case strutPosition config of
          TopPos -> Gtk.OrientationHorizontal
          BottomPos -> Gtk.OrientationHorizontal
          _ -> Gtk.OrientationVertical
  tray <- buildTrayWithHost orientation
  window <- buildStrutWindow config
  Gtk.containerAdd window tray
  Gtk.widgetShowAll window
  Gtk.onWidgetDestroy window Gtk.mainQuit
  return ()

main :: IO ()
main = do
  Gtk.init Nothing
  join $ execParser $ info (parser <**> helper)
               (  fullDesc
               <> progDesc "Run a standalone StatusNotifierItem/AppIndicator tray.")
  Gtk.main
