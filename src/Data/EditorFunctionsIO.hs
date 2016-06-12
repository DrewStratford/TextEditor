module Data.EditorFunctionsIO
       ( drawTextScreen
       , drawSection
       ) where


import Data.Foldable
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M


import Data.TextBuffer

import Editor.EditorTypes
import Editor.Editor
import Editor.Modes
import Editor.TextDisplay

import qualified Graphics.Vty as Vty


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
drawSection :: TextBuffer -> Int -> Vty.Image
drawSection text width = go lines
  where lines      = toList $ fmap toList (merge text)
        go []      = Vty.emptyImage
        go ("":ss) = Vty.string Vty.defAttr " " Vty.<-> go ss
        go (s:ss)  = Vty.string Vty.defAttr s Vty.<-> go ss

drawLine' :: Int -> String -> Vty.Image
drawLine' _ [] = undefined
drawLine' i (c:cs)
  | i < 0 = undefined
  | i < 4 && c /= '\t' = undefined
  | otherwise = case c of
    '\t' ->  undefined
    _    ->  undefined

drawTextScreen :: Int -> Int -> Vty.Vty -> Editor -> Vty.Picture
drawTextScreen width height vty editor = 
  let editor' = scrollScreen (height, width) editor
      (l,c) = getLineCol $ text td
      tLine = topLine td
      lCol  = leftCol td
      td    = getTextDisplay editor' 
      drawingSection = getSection (lCol,tLine) (height-1,width-1) (text td) 
      outputimg      = drawSection drawingSection (width -1)
  in Vty.picForImage outputimg 
  

