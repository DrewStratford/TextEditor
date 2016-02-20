module Data.EditorFunctions where

import Data.Foldable
import Data.Maybe
import Control.Monad.State
import qualified Data.Map as M

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Data.TextBuffer
import Editor.Editor
import Editor.TextDisplay

getLineColumn editor = getLineCol $ text $ getTextDisplay editor

toText :: (TextBuffer -> TextBuffer) -> Editor -> Editor
toText f editor =
  (modifyTextDisplay $ setColAlign $ snd $ getLineColumn editor) $
  modifyTextDisplay (modifyText f) editor

insertClipBoard :: Editor -> Editor
insertClipBoard editor = toText (`insertSection` insertee) editor
  where insertee = getClipBoard editor

doOnVisualRange :: ((Int, Int) -> (Int, Int) -> Editor -> Editor) -> Editor -> Editor
doOnVisualRange f editor =
  let startPoint = getLineColumn editor
  in case getMode $ getTextDisplay editor of
     (Visual endPoint) -> f startPoint endPoint editor
     _                 -> editor

copyToClipBoard :: Editor -> Editor
copyToClipBoard = doOnVisualRange go
  where go start end editor = setClipboard section editor
          where section = removeSection start end (text $ getTextDisplay editor)

cutToClipBoard :: Editor -> Editor
cutToClipBoard = doOnVisualRange go . copyToClipBoard
  where go start end = modifyTextDisplay (modifyText $ removeSection start end)

        
