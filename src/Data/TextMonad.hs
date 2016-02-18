module Data.TextMonad
       ( Mode (..)
       , TextM
       , TextDisplay
--       , Key(..)
       , toText
       , output
       , moveColumn
       , setMode
       , moveLine
       , getMode
       , runTextM
       , createTextDisplay
       , getText
       , putText
       , getBufferList
       , putBufferList
       , getInput
       , setMark
       , getMark
       , insertClipBoard
       , copyToClipBoard
       , cutToClipBoard
       , getLineColumn
       , moveToEnd
       ) where


import Data.Foldable
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Data.TextBuffer
import Editor.Editor
import Editor.TextDisplay

--------------------------------------------------------------------------------------------

  
--TODO replace list with something more suitable
type TextM a = StateT Editor IO a

getText :: TextM TextDisplay
getText = fmap getTextDisplay get

getBufferList :: TextM (M.Map String TextDisplay)
getBufferList = fmap getBuffers get

putText :: TextDisplay -> TextM ()
putText text = do
  editor <- get
  put editor { getTextDisplay = text }

putBufferList :: M.Map String TextDisplay -> TextM ()
putBufferList bufferList = do
  editor <- get
  put editor { getBuffers = bufferList }

--------------------------------------------------------------------------------------------
toText :: (TextBuffer -> TextBuffer) -> TextM ()
toText f = do
  t <- getText
  let text' = f $ text t
  -- set the cursor alignment
  (_, colAlign') <- getLineColumn
  putText t{ text = text' , colAlign = colAlign'}
  return ()

output :: TextM()
output = do
  scrollScreen
  td <- getText
  let cursor@(l,c) = getLineCol $ text td
      tLine = topLine td
      lCol  = leftCol td
  (height, width) <- lift scrSize
  lift $  do wMove (window td) 0 0 
             drawSection (text td) lCol tLine (width-1) (height-1)

   -- if in visual mode draw highlighted area
  case getMode td of
    (Visual selectEnd@(sLine, sCol)) -> do
      let highlightSection = getSection cursor selectEnd (text td) 
      lift $  do setStyle highlightStyle
                 wMove (window td) (min (sLine-tLine) (l-tLine)) (min (sCol-lCol) (c-lCol))
                 drawSection highlightSection lCol tLine (width-1) (height-1)
                 setStyle defaultCursesStyle
    _                 -> return ()
  lift $ do wMove (window td) l c
            refresh

-- | scrolls the screen based on cursor position
-- TODO: make this legible
scrollScreen :: TextM ()
scrollScreen = do
  td <- getText
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
  
  putText td { topLine = tLine', leftCol = lCol'}
  return ()

setMode :: Mode -> TextM ()
setMode newMode = do
  td <- getText
  putText td { getMode = newMode }

setMark :: (Int, Int) -> String -> TextM ()
setMark pos label = do
  td <- getText
  let ms = M.insert label pos (marks td) 
  putText td { marks = ms}

getMark :: String -> TextM (Maybe (Int, Int))
getMark label = do
  td <- getText
  return $ label `M.lookup` marks td

insertClipBoard :: TextM ()
insertClipBoard = do
  editor <- get
  let inserting = getClipBoard editor
  toText (`insertSection` inserting)

cutToClipBoard :: TextM ()
cutToClipBoard = do
  td       <- getText
  endPoint <- getLineColumn
  case getMode td of
    Visual startPoint -> do
      let text' = removeSection startPoint endPoint (text td)
          clipBoard' = getSection startPoint endPoint (text td)
      toText $ const text'
      editor <- get
      put editor { getClipBoard = clipBoard' }
      
    _ -> return ()
     
  
 


copyToClipBoard :: TextM ()
copyToClipBoard = do
  td <- getText
  editor <- get
  startPoint <- getLineColumn
  case getMode td of
    Visual endPoint -> do
      let clipBoard' = getSection startPoint endPoint (text td)
      put editor { getClipBoard = clipBoard' }
    _              -> return ()
  
moveColumn :: Int -> TextM ()
moveColumn delta = do
  (_, c) <- getLineColumn
  toText $ \t -> moveCol t (c + delta) 
  
moveLine :: Int -> TextM ()
moveLine delta = do
  td <- getText
  let (l,_) = getLineCol (text td)
      col   = colAlign td
  toText $ \t -> moveLineCol t (l + delta) col

getInput :: TextM Key
getInput = lift getCh


getLineColumn :: TextM (Int, Int)
getLineColumn = do
  td <- getText
  return $ getLineCol $ text td
  
moveToEnd = toText moveToEndOfLine
-------------------------------------------------------------------------------------
--  curses window, TextBuffer to draw from, x, y, width of section, height of section
drawSection :: TextBuffer -> Int -> Int -> Int -> Int -> IO ()
drawSection text x y width height = do
  let lines :: [String]
      lines       =  toList $ fmap toList (getLineSection y height text)
      go []     _ = return ()
      go [s]    _ = drawLine (min width (length s)) s
      go (s:ss) c = do
        drawLine (min width (length s)) s
        drawLine 1 "\n"
        go ss (c + 1)
  go lines x

  


-------------------------------------------------------------------------------------
-- constructors etc

runTextM :: TextM a -> Editor -> IO ()
runTextM procedure textDisplay = do
  --echo False
  initCurses
  raw True
  evalStateT procedure textDisplay
  endWin
  
--------------------------------------------------------------------------------------------
 
-- styles
highlightStyle = mkCursesStyle [Underline]
