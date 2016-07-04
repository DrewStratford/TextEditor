module Editor.Editor
       ( Editor (..)
       , editor 
       , setTextDisplay
       , setClipboard
       , modifyTextDisplay
       , modifyClipBoard
       , endSession
       ) where

import qualified Data.Map as M

import Data.TextBuffer

import Editor.EditorTypes
import Editor.FrameList

--------------------------------------------------------------------------------
-- setters and modifiers
setTextDisplay  insertee editor = editor{ getTextDisplay = insertee }
setClipboard    insertee editor = editor{ getClipBoard   = insertee }

modifyTextDisplay  f editor = editor{ getTextDisplay = f $ getTextDisplay editor }
modifyClipBoard    f editor = editor{ getClipBoard   = f $ getClipBoard editor }

endSession editor = editor{ isFinished = True}
--------------------------------------------------------------------------------

editor :: TextDisplay state -> Int -> Int -> Editor state 
editor text height width = Editor text (fromStrings []) height width
     

addToClipBoard :: Editor state -> TextBuffer -> Editor state
addToClipBoard editor text = editor { getClipBoard = text }
