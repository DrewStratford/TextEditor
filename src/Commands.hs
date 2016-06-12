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

runEditorCommand :: EditorCommand -> Editor -> Editor
runEditorCommand (EditorCommand editCmd) = run editCmd


{- | turns a list of key bindings into KeyBinds
     which are used by Modes to store their KeyBinds
-}
makeKeyBinds :: [BindKey] -> KeyBinds
makeKeyBinds keys = go keys M.empty
  where go :: [BindKey] -> KeyBinds -> KeyBinds
        go []  m                        = m
        go (BindKey key mod binding :kbs) m =
          go kbs (M.insert (key, mod) (EditorCommand binding) m)
