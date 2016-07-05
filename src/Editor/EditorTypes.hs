module Editor.EditorTypes where

import qualified Data.Map as M
 
import Data.TextBuffer
import Editor.FrameList

import qualified Graphics.Vty as Vty

data TextDisplay state = TextDisplay
  { text     :: TextBuffer
  , topLine  :: Int
  , leftCol  :: Int
  , state    :: state
  , getMode  :: Mode state
  , marks    :: M.Map String (Int, Int)
  , colAlign :: Int
  , filePath :: FilePath
  , getTabInd   :: Int
  }

data Mode state =  Mode
  { keyBindings :: KeyBinds state
  , outputState :: Editor state -> EditorOutput
  }


data Editor state = Editor 
  { getTextDisplay :: TextDisplay state
  , getClipBoard   :: TextBuffer
  , scrnHeight     :: Int
  , scrnWidth      :: Int
  }

{- | EditorOutput is the struct that will be returned for each stage in the stream
     will contain data for outputting the screen etc
-}
data EditorOutput = EditorOutput
  { getPicture    :: Vty.Picture
  , isFinished :: Bool
    -- TODO: Expand with basic events like saving and opening
  }
{- |
   The Command class runs the function (c -> c) on the 'c' component
   of the editor. Assuming 'c' is a component of the editor. Run should
   access the apropriate appropriate 'c' in the editor

-}
class Command c where
  run :: (c -> b) -> Editor a -> Editor b


type Action a = [Vty.Event] -> Editor a -> [EditorOutput]

{- | BindKey is used to store keybinds as a combination of some key
     and and a command. It is existential so that we cann easily
     specify a collection of keybinds without having to care
     to much about the type of the command.
-}

{-
bindKey :: Command c =>
           Key -> [Modifier] -> (c -> c) -> (Event, Action a)
bindKey key mods command = bindEvent event command
  where event = EvKey key mods

bindEvent :: Command c =>
           Event  -> (c -> c) -> (Event, Action a)
bindEvent event command = 
  let f      = run command
      action keys editor = performContinuation f keys editor
  in (event, action)

performContinuation :: (Editor a -> Editor b) -> Action a
performContinuation f [] editor = undefined --output f editor
performContinuation f (event: events) editor =
  let editor'      = f editor
      mode         = getMode $ getTextDisplay editor'
      nextAction   = event `M.lookup` keyBindings mode
      editorOutput = undefined
  in case nextAction of
          -- ignores current keystroke when no action TODO: REWORK
          Nothing     -> performContinuation f events editor 
          Just action -> editorOutput : action events editor'
     
outputEditor :: Editor a -> EditorOutput
outputEditor editor = EditorOutput pic False
  where pic = updateImage editor
-}
{- | A mapping of keys to keybindings
-}
type KeyBinds  state =  M.Map Vty.Event (Action state)
