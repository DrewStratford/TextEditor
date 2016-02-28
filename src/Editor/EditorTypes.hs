{-# LANGUAGE  ExistentialQuantification, Rank2Types #-}
module Editor.EditorTypes where

import qualified Data.Map as M
 
import Data.TextBuffer
import Editor.FrameList

import KeyInput

data TextDisplay = TextDisplay
  { text     :: TextBuffer
  , topLine  :: Int
  , leftCol  :: Int
  , getMode  :: EditorMode
  , marks    :: M.Map String (Int, Int)
  , colAlign :: Int
  }

class Mode a where
   -- returns the appropriate command based on the state of c
   getCommand :: Key -> a -> Editor -> Maybe Editor
   -- updates the state based on the given key
   updateState :: Key -> a -> a
   keyBindings :: a -> KeyBinds
   outputState :: (Int,Int) -> a -> IO ()

data EditorMode = forall m. Mode m => EditorMode m

data Editor = Editor 
  { getFrame       :: Frame
  , getBuffers     :: M.Map String TextDisplay
  , getTextDisplay :: TextDisplay
  , getClipBoard   :: TextBuffer
  }

{- |
   The Command class runs the function (c -> c) on the 'c' component
   of the editor. Assuming 'c' is a component of the editor. Run should
   access the apropriate appropriate 'c' in the editor

   The Maybe is to account for any error or termination that may occur
   (will probably change to Either or something more appropriate.)
-}
class Command c where
  run :: (c -> c) -> Editor -> Maybe Editor


{-| Editor Command is used to group all functions that can be performed
   on an editor in the same collection, (eg. a  collection of keybinds)
-}
data EditorCommand = forall c. (Command c) => EditorCommand (c -> c)

{- | BindKey is used to store keybinds as a combination of some key
     and and a command. It is existential so that we cann easily
     specify a collection of keybinds without having to care
     to much about the type of the command.
-}
data BindKey = forall c. Command c => BindKey Key (c -> c)

{- | A mapping of keys to keybindings
-}
type KeyBinds =  M.Map Key EditorCommand
