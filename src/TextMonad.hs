module TextMonad
       ( Mode (..)
       , TextM
       , toText
       , output
       , moveColumn
       , setMode
       , moveLine
       , mode
       , runTextM
       , createTextDisplay
       ) where


import Data.Foldable
import Control.Monad.State
import qualified Data.Map as M

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Data.TextBuffer

data Mode = Insert | Normal | Command | Visual

data TextDisplay = TextDisplay
  { text     :: TextBuffer
  , topLine  :: Int
  , leftCol  :: Int
  , window   :: Window
  , mode     :: Mode
  , marks    :: M.Map String (Int, Int)
  , colAlign :: Int
  }
  
type TextM a = StateT TextDisplay IO a

toText :: (TextBuffer -> TextBuffer) -> TextM ()
toText f = do
  t <- get
  let text' = f $ text t
  put t{ text = text' }
  return ()

output :: TextM()
output = do
  scrollScreen
  td <- get
  let (l,c) = getLineCol $ text td
      tLine = topLine td
      lCol  = leftCol td
  (height, width) <- lift scrSize
  lift $  do wMove (window td) 0 0 
             drawSection (window td) (text td) lCol tLine (width-1) (height-1)
             wMove (window td) (l - tLine) (c - lCol)
             wRefresh $ window td

-- | scrolls the screen based on cursor position
scrollScreen :: TextM ()
scrollScreen = do
  td <- get
  let (cursLine, cursCol) = getLineCol $ text td
      tLine               = topLine td
      lCol                = leftCol td

  (scrLines, scrCols) <- lift scrSize

  let tLine'
       | cursLine > tLine + scrLines - 1 = tLine + (cursLine - (tLine +scrLines - 1))
       | otherwise = min cursLine tLine
      lCol'
       | cursCol > lCol + scrCols - 1 = lCol + (cursCol - (lCol + scrCols - 1))
       | otherwise = min cursCol lCol
  
  put td { topLine = tLine', leftCol = lCol'}
  return ()

setMode :: Mode -> TextM ()
setMode newMode = do
  td <- get
  put td { mode = newMode }

setMark :: (Int, Int) -> String -> TextM ()
setMark pos label = do
  td <- get
  let ms = M.insert label pos (marks td) 
  put td { marks = ms}

getMark :: String -> TextM (Maybe (Int, Int))
getMark label = do
  td <- get
  return $ label `M.lookup` marks td

moveColumn :: Int -> TextM ()
moveColumn delta = do
  td <- get
  let (_,c) = getLineCol (text td)
      col   = colAlign td
  toText $ \t -> moveCol t (c + delta) 
  td <- get
  -- we change the column we "aim" for when moving lines
  let (_,c) = getLineCol (text td)
  put td { colAlign = c }
  
moveLine :: Int -> TextM ()
moveLine delta = do
  td <- get
  let (l,_) = getLineCol (text td)
      col   = colAlign td
  toText $ \t -> moveLineCol t (l + delta) col
-------------------------------------------------------------------------------------
--  curses window, TextBuffer to draw from, x, y, width of section, height of section
drawSection :: Window -> TextBuffer -> Int -> Int -> Int -> Int -> IO ()
drawSection window text x y width height = do
  let lines :: [String]
      lines =  toList $ fmap toList (getLineSection y height text)
      go [] = return ()
      go (s:ss) = do
        (drawLine width . drop x) s
        drawLine 1 "\n"
        go ss
  go lines

-------------------------------------------------------------------------------------

createTextDisplay :: FilePath -> IO TextDisplay
createTextDisplay filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  window <- initScr
  return $ TextDisplay textBuffer 0 0 window Normal M.empty 0

runTextM :: TextM a -> TextDisplay -> IO ()
runTextM procedure textDisplay = do
  echo False
  initCurses
  evalStateT procedure textDisplay
  endWin
  
