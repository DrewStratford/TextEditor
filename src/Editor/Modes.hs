module Editor.Modes
       ( Mode (..)
       , lookUpKey
       , getKeyBinding
       , drawMode
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands
import KeyInput

import Editor.Editor
import Editor.TextDisplay
import Editor.EditorTypes

  -- | updates state and runs keybind on editor
getKeyBinding :: Key -> EditorMode -> Editor -> Editor
getKeyBinding key (EditorMode mode) editor = getCommand key mode $ update editor
  where update :: Editor -> Editor
        update = modifyTextDisplay $ setGetMode (EditorMode $ updateState key mode)


lookUpKey :: Mode m => Key -> m -> Maybe (Editor -> Editor)
lookUpKey key mode = fmap runEditorCommand editorCommand
  where editorCommand = M.lookup key $ keyBindings mode
        
drawMode (EditorMode mode) scrnSize = outputState scrnSize mode
