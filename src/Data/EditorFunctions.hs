module Data.EditorFunctions
       ( toText
       , getLineColumn
       , insertClipBoard
--       , copyToClipBoard
--       , cutToClipBoard
       , moveLine
       , moveColumn
       , padString
       , getPadding
       , getLineAt
       ) where

import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Data.TextBuffer
import Editor.EditorTypes
import Editor.Editor
import Editor.TextDisplay

getLineColumn editor = getLineCol $ text $ getTextDisplay editor

-- | modifies the textBuffer while also updating the column alignment (this
--   should be used over modifyText in most cases
toText :: (TextBuffer -> TextBuffer) -> Editor -> Editor
toText f editor =
  (modifyTextDisplay $ setColAlign $ snd $ getLineColumn editor) $
  modifyTextDisplay (modifyText f) editor

insertClipBoard :: Editor -> Editor
insertClipBoard editor = toText (`insertSection` insertee) editor
  where insertee = getClipBoard editor

        {-
doOnVisualRange :: ((Int, Int) -> (Int, Int) -> Editor -> Editor) -> Editor -> Editor
doOnVisualRange f editor =
  let startPoint = getLineColumn editor
  in case startOfRange $ getMode $ getTextDisplay editor of
     (Just endPoint) -> f startPoint endPoint editor
     _               -> editor

copyToClipBoard :: Editor -> Editor
copyToClipBoard = doOnVisualRange go
  where go start end editor = setClipboard section editor
          where section = getSection start end (text $ getTextDisplay editor)

cutToClipBoard :: Editor -> Editor
cutToClipBoard = doOnVisualRange go . copyToClipBoard
  where go start end = modifyTextDisplay (modifyText $ removeSection start end)
        -}

moveColumn delta editor = toText (`moveCol` (col + delta)) editor        
  where (_, col) = getLineColumn editor


moveLine :: Int -> TextDisplay -> TextDisplay
moveLine delta textDis = modifyText (\t -> moveLineCol t (line + delta) col) textDis        
  where (line,_) = getLineCol $ text textDis
        col      = colAlign textDis

-- | TODO: COMMENT IS INACCURATE is 1 indexed as this seems to make more sense
getLineAt :: Editor -> Int -> String
getLineAt ed line = if null seq then "" else toList (Seq.index seq 0)
  where line' = line 
        seq   = getLineSection line' 1 (text $ getTextDisplay ed)
{- | used to handle tabs etc -}

padString :: Int -> String -> String
padString indent = concatMap expand
  where expand '\t' = [' ' | _ <- [0 .. indent]]
        expand c    = [c]

getPadding :: TextDisplay -> Int -> String -> Int
getPadding td cursor = length . padString indent . take cursor
  where indent = getTabInd td

