module TextMonad
       ( Mode (..)
       , TextM
--       , Key(..)
       , toText
       , output
       , moveColumn
       , setMode
       , moveLine
       , getMode
       , runTextM
       , createTextDisplay
       , getInput
       , setMark
       , getMark
       , insertClipBoard
       , copyToClipBoard
       , cutToClipBoard
       , getLineColumn
       ) where


import Data.Foldable
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Data.TextBuffer

--------------------------------------------------------------------------------------------
data Mode = Insert | Normal | Command | Visual (Int, Int) deriving (Show, Eq)

data TextDisplay = TextDisplay
  { text     :: TextBuffer
  , topLine  :: Int
  , leftCol  :: Int
  , window   :: Window
  , getMode  :: Mode
  , marks    :: M.Map String (Int, Int)
  , colAlign :: Int
  , clipBoard :: TextBuffer
  }
  
type TextM a = StateT TextDisplay IO a

--------------------------------------------------------------------------------------------
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
             drawSection (text td) lCol tLine (width-1) (height-1)
             wMove (window td) (height-1) 0
             drawLine 80 $ show (getMode td) ++ " <" ++ show l ++ ", " ++ show c ++ ">"
             wMove (window td) (l - tLine) (c - lCol)
             wRefresh $ window td

-- | scrolls the screen based on cursor position
-- TODO: make this legible
scrollScreen :: TextM ()
scrollScreen = do
  td <- get
  let (cursLine, cursCol) = getLineCol $ text td
      tLine               = topLine td
      lCol                = leftCol td

  (scrLines, scrCols) <- lift scrSize
  -- changes the top drawing section (determined by topline + height and leftCol + width)
  -- if the cursor is outside of the box
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
  put td { getMode = newMode }

setMark :: (Int, Int) -> String -> TextM ()
setMark pos label = do
  td <- get
  let ms = M.insert label pos (marks td) 
  put td { marks = ms}

getMark :: String -> TextM (Maybe (Int, Int))
getMark label = do
  td <- get
  return $ label `M.lookup` marks td

insertClipBoard :: TextM ()
insertClipBoard = do
  td <- get
  let inserting = clipBoard td
  toText (`insertSection` inserting)

cutToClipBoard :: TextM ()
cutToClipBoard = do
  td       <- get
  endPoint <- getLineColumn
  case getMode td of
    Visual startPoint -> do
      let text' = removeSection startPoint endPoint (text td)
          clipBoard' = getSection startPoint endPoint (text td)
      put $ td { text = text', clipBoard = clipBoard'}
      
    _ -> return ()
     
  
 

copyToClipBoard :: TextM ()
copyToClipBoard = do
  td <- get
  startPoint <- getLineColumn
  case getMode td of
    Visual endPoint -> do
      let clipBoard' = getSection startPoint endPoint (text td)
      put $ td { clipBoard = clipBoard' }
    _              -> return ()
    
  
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

getInput :: TextM Key
getInput = lift getCh


getLineColumn :: TextM (Int, Int)
getLineColumn = do
  td <- get
  return $ getLineCol $ text td
  
-------------------------------------------------------------------------------------
--  curses window, TextBuffer to draw from, x, y, width of section, height of section
drawSection :: TextBuffer -> Int -> Int -> Int -> Int -> IO ()
drawSection text x y width height = do
  let lines :: [String]
      lines =  toList $ fmap toList (getLineSection y height text)
      go [] _ = return ()
      go (s:ss) c= do
        -- TODO: draw line nums properly
        (drawLine width . drop x) s
        drawLine 1 "\n"
        go ss (c + 1)
  go lines x

-------------------------------------------------------------------------------------
-- constructors etc
createTextDisplay :: FilePath -> IO TextDisplay
createTextDisplay filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  window <- initScr
  return $ TextDisplay textBuffer 0 0 window Normal M.empty 0 (fromStrings [])

runTextM :: TextM a -> TextDisplay -> IO ()
runTextM procedure textDisplay = do
  --echo False
  initCurses
  raw True
  evalStateT procedure textDisplay
  endWin
  
--------------------------------------------------------------------------------------------
 
