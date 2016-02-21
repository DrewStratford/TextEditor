{-# LANGUAGE  ExistentialQuantification #-}
module Commands
       ( BindKey (..)
       , KeyBinds
       , EditorCommand
       , Command (..)
       , runEditorCommand
       , makeKeyBinds
       ) where

import qualified Data.Map as M

import Editor.Editor
import KeyInput

data EditorCommand = forall c. (Command c) => EditorCommand (c -> c)

class Command c where
  run :: (c -> c) -> Editor -> Editor

runEditorCommand :: EditorCommand -> Editor -> Editor
runEditorCommand (EditorCommand editCmd) = run editCmd

data BindKey = forall c. Command c => BindKey Key (c -> c)

type KeyBinds = M.Map Key EditorCommand 

makeKeyBinds :: [BindKey] -> KeyBinds
makeKeyBinds keys = go keys M.empty
  where go :: [BindKey] -> KeyBinds -> KeyBinds
        go []  m                        = m
        go (BindKey key binding :kbs) m =
          go kbs (M.insert key (EditorCommand binding) m)
