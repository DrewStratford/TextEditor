module Editor.Modes
       ( Mode (..)
       , lookUpKey
       , getKeyBinding
       , drawMode
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands
import Graphics.Vty (Key(..), Modifier(..))

import Editor.Editor
import Editor.TextDisplay
import Editor.EditorTypes

  -- | updates state and runs keybind on editor
getKeyBinding :: Key -> [Modifier] -> EditorMode -> Editor -> Editor
getKeyBinding key mod (EditorMode mode) editor = getCommand key mod mode $ update editor
  where update :: Editor -> Editor
        update = modifyTextDisplay $ setGetMode (EditorMode $ updateState key mode)


lookUpKey :: Mode m => Key -> [Modifier] -> m -> Maybe (Editor -> Editor)
lookUpKey key mods mode = fmap runEditorCommand editorCommand
  where editorCommand = M.lookup (key, mods) $ keyBindings mode
        
drawMode (EditorMode mode) scrnSize = outputState scrnSize mode
