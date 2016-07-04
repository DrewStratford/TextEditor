module Editor.Modes
       ( Mode (..)
       , bindKey
       , bindEvent
       , makeKeyBinds
       , startStream
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands
import Graphics.Vty (Key(..), Event(..), Modifier(..))

import Editor.Editor
import Editor.TextDisplay
import Editor.EditorTypes


{- | BindKey is used to store keybinds as a combination of some key
     and and a command. It is existential so that we cann easily
     specify a collection of keybinds without having to care
     to much about the type of the command.
-}
{- old version that uses the Command interface
bindKey :: Command c => Key -> [Modifier] -> (c -> c) -> (Event, Action a)
bindKey key mods command = bindEvent event command
  where event = EvKey key mods

bindEvent :: Command c => Event  -> (c -> c) -> (Event, Action a)
bindEvent event command = 
  let f      = run command
      action keys editor = performContinuation f keys editor
  in (event, action)
-}

bindKey :: Key -> [Modifier] -> (Editor a -> Editor b) -> (Event, Action a)
bindKey key mods command = bindEvent event command
  where event = EvKey key mods

bindEvent :: Event  -> (Editor a -> Editor b) -> (Event, Action a)
bindEvent event command = 
  let action keys editor = performContinuation command keys editor
  in (event, action)

performContinuation :: (Editor a -> Editor b) -> Action a
performContinuation f [] editor = 
  let mode         = getMode $ getTextDisplay editor
  in [outputState mode editor]
performContinuation f (event: events) editor =
  let editor'      = f editor
      mode         = getMode $ getTextDisplay editor'
      nextAction   = event `M.lookup` keyBindings mode
      editorOutput = outputState mode editor'
  in case nextAction of
          -- ignores current keystroke when no action TODO: REWORK
          Nothing     -> performContinuation f events editor 
          Just action -> editorOutput : action events editor'
     
makeKeyBinds :: Ord k => [(k, a)] -> M.Map k a
makeKeyBinds = M.fromList

{-| starts the "loop" |-}
startStream :: [Event] -> Editor a -> [EditorOutput]
startStream = performContinuation id
  
