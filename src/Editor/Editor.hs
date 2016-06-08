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
       , setPendingIO
       , modifyPendingIO
       , endSession
       ) where

import qualified Data.Map as M

import Data.TextBuffer

import Editor.EditorTypes
import Editor.FrameList

--------------------------------------------------------------------------------
-- setters and modifiers
setFrame        insertee editor = editor{ getFrame       = insertee }
setBuffers      insertee editor = editor{ getBuffers     = insertee }
setTextDisplay  insertee editor = editor{ getTextDisplay = insertee }
setClipboard    insertee editor = editor{ getClipBoard   = insertee }
setPendingIO    insertee editor = editor{ getPendingIO   = insertee }

modifyFrame            f editor = editor{ getFrame       = f $ getFrame editor }
modifyBuffers          f editor = editor{ getBuffers     = f $ getBuffers editor }
modifyTextDisplay      f editor = editor{ getTextDisplay = f $ getTextDisplay editor }
modifyClipBoard        f editor = editor{ getClipBoard   = f $ getClipBoard editor }
modifyPendingIO        f editor = editor{ getPendingIO   = f $ getPendingIO editor }

endSession editor = editor{ isFinished = True}
--------------------------------------------------------------------------------

editor :: TextDisplay -> Editor 
editor text =
  let name = "Scratch"
      bs   = M.insert name text M.empty
      f    = toZip name
  in Editor f bs text (fromStrings []) (return ()) False
     
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
