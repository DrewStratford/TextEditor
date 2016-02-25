module Editor.Modes
       ( Mode (..)
       , defaultLookUp
       , getBinding
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands
import KeyInput
import Editor.EditorTypes


getBinding :: Mode -> Key -> (Editor -> Maybe Editor)
getBinding mode key = fromMaybe return (f key bindings)
  where f        = keyLookUp mode
        bindings = keyBindings mode


defaultLookUp :: Key -> KeyBinds -> Maybe (Editor -> Maybe Editor)
defaultLookUp key bindings = runEditorCommand <$> M.lookup key bindings
