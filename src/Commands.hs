module Commands
       ( BindKey (..)
       , KeyBinds
       , EditorCommand
       , Command (..)
       , runEditorCommand
       , makeKeyBinds
       ) where

import qualified Data.Map as M

import Editor.EditorTypes

runEditorCommand :: EditorCommand -> Editor -> Maybe Editor
runEditorCommand (EditorCommand editCmd) = run editCmd


{- | turns a list of key bindings into KeyBinds
     which are used by Modes to store their KeyBinds
-}
makeKeyBinds :: [BindKey] -> KeyBinds
makeKeyBinds keys = go keys M.empty
  where go :: [BindKey] -> KeyBinds -> KeyBinds
        go []  m                        = m
        go (BindKey key binding :kbs) m =
          go kbs (M.insert key (EditorCommand binding) m)