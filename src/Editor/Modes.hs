module Editor.Modes
       ( Mode (..)
       , lookUpKey
       , getKeyBinding
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands
import KeyInput
import Editor.EditorTypes

getKeyBinding :: Key -> EditorMode -> Maybe (Editor -> Maybe Editor)
getKeyBinding key (EditorMode mode) = lookUpKey key mode

lookUpKey :: Mode m => Key -> m -> Maybe (Editor -> Maybe Editor)
lookUpKey key mode = fmap runEditorCommand editorCommand
  where editorCommand = M.lookup key $ keyBindings mode
        
