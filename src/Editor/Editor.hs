module Editor.Editor
       ( Editor (..)
       , editor 
       , setTextDisplay
       , setClipboard
       , modifyTextDisplay
       , modifyClipBoard
       , endSession
       , setState
       , setMode
       , modifyState
       ) where

import qualified Data.Map as M

import Data.TextBuffer

import Editor.EditorTypes
import Editor.FrameList

--------------------------------------------------------------------------------
-- setters and modifiers
setTextDisplay  insertee editor = editor{ getTextDisplay = insertee }
setClipboard    insertee editor = editor{ getClipBoard   = insertee }
setState           state editor = editor{ getState       = state}
setMode      state mode editor = editor{ getState       = state, getMode = mode }

modifyTextDisplay  f editor = editor{ getTextDisplay = f $ getTextDisplay editor }
modifyClipBoard    f editor = editor{ getClipBoard   = f $ getClipBoard editor }
modifyState        f editor = editor{ getState       = f $ getState editor }

endSession editor = editor{ isFinished = True}
--------------------------------------------------------------------------------

editor :: TextDisplay -> state -> Mode state -> Int -> Int -> Editor state 
editor text state mode height width = Editor text (fromStrings []) height width state mode
     

addToClipBoard :: Editor state -> TextBuffer -> Editor state
addToClipBoard editor text = editor { getClipBoard = text }
