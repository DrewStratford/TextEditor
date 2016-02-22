module Data.EditorFunctionsIO
       ( output
       ) where


import Data.Foldable
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Data.TextBuffer

import Editor.EditorTypes
import Editor.Editor
import Editor.TextDisplay


-- | scrolls the display based on the size of the window
scrollScreen :: (Int, Int) -> Editor -> Editor
scrollScreen (scrLines, scrCols) editor = modifyTextDisplay (setTopLine tLine . setLeftCol lCol') editor
  where (cursLine, cursCol) = getLineCol $ text textDisplay
        tLine               = topLine textDisplay
        lCol                = leftCol textDisplay
        textDisplay         = getTextDisplay editor

        -- changes the top drawing section (determined by topline + height and leftCol + width)
        -- if the cursor is outside of the box
        tLine'
          | cursLine > tLine + scrLines - 1 = tLine + (cursLine - (tLine +scrLines - 1))
          | otherwise = min cursLine tLine
        lCol'
          | cursCol > lCol + scrCols - 1 = lCol + (cursCol - (lCol + scrCols - 1))
          | otherwise = min cursCol lCol
  

--  TextBuffer to draw from, x, y, width of section, height of section
drawSection :: TextBuffer -> Int -> IO ()
drawSection text width = do
  let lines     =   toList $ fmap toList (merge text)
      go []     = return ()
      go [s]    = drawLine (min width (length s)) s
      go (s:ss) = do
        drawLine (min width (length s)) s
        drawLine 1 "\n"
        go ss
  go lines

drawLine' :: Int -> String -> IO ()
drawLine' _ [] = return ()
drawLine' i (c:cs)
  | i < 0 = return ()
  | i < 4 && c /= '\t' = drawLine 1 [c] >> drawLine' (i - 4) cs
  | otherwise = case c of
    '\t' -> drawLine 4 "    " >> drawLine' (i - 4) cs
    _    -> drawLine 1 [c] >> drawLine' (i - 1) cs

output :: Editor -> IO ()
output editor = do
  (height, width) <- scrSize
  let editor' = scrollScreen (height, width) editor
      cursor@(l,c) = getLineCol $ text td
      tLine = topLine td
      lCol  = leftCol td
      td    = getTextDisplay editor' 
      drawingSection = getSection (lCol,tLine) (height-1,width-1) (text td) 
  move 0 0 
  drawSection drawingSection (width -1)
  
   -- if in visual mode draw highlighted area
  
  case startOfRange $ getMode td of
    (Just selectEnd@(sLine, sCol)) -> do
      let highlightSection = getSection cursor selectEnd (text td) 
      setStyle highlightStyle
      move (min (sLine-tLine) (l-tLine)) (min (sCol-lCol) (c-lCol))
      drawSection highlightSection (width-1)
      setStyle defaultCursesStyle
    _                 -> return ()
  
  move l c
  refresh

-- styles
highlightStyle = mkCursesStyle [Underline]
