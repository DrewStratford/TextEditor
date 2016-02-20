module Editor.Editor
       ( Editor (..)
       , addSplit
       , storeText
       , editor 
       , setFrame
       , setBuffers
       , setTextDisplay
       , setClipboard
       , modifyFrame
       , modifyBuffers            
       , modifyTextDisplay
       , modifyClipBoard
       ) where

import Editor.FrameList

import qualified Data.Map as M
--import Data.Maybe

import Data.TextBuffer
import Editor.TextDisplay

data Editor = Editor
    { getFrame     :: Frame
    , getBuffers   :: M.Map String TextDisplay
    , getTextDisplay      :: TextDisplay
    , getClipBoard :: TextBuffer
    }
--------------------------------------------------------------------------------
-- setters and modifiers
setFrame        insertee editor = editor{ getFrame       = insertee }
setBuffers      insertee editor = editor{ getBuffers     = insertee }
setTextDisplay  insertee editor = editor{ getTextDisplay = insertee }
setClipboard    insertee editor = editor{ getClipBoard   = insertee }

modifyFrame       f editor = editor{ getFrame        = f $ getFrame editor }
modifyBuffers     f editor = editor{ getBuffers      = f $ getBuffers editor }
modifyTextDisplay f editor = editor{ getTextDisplay  = f $ getTextDisplay editor }
modifyClipBoard   f editor = editor{ getClipBoard    = f $ getClipBoard editor }

--------------------------------------------------------------------------------

editor :: TextDisplay -> Editor
editor text =
  let name = "Scratch"
      bs   = M.insert name text M.empty
      f    = toZip name
  in Editor f bs text (fromStrings [])
     
addSplit :: Editor -> String -> TextDisplay -> SplitType -> Editor
addSplit editor name buffer splitType =
  let buffers' = M.insert name buffer (getBuffers editor)
      frame'   = split (getFrame editor) splitType name
   in (storeText editor) { getBuffers = buffers', getFrame = frame' }

-- | updates the buffers to store the current version of the textBuffer
storeText editor =
  let name = getAtWindow $ getFrame editor
      buffers = maybe (getBuffers editor)
                (\n -> M.insert n (getTextDisplay editor) (getBuffers editor))
                name
   in editor { getBuffers = buffers }

addToClipBoard :: Editor -> TextBuffer -> Editor
addToClipBoard editor text = editor { getClipBoard = text }
