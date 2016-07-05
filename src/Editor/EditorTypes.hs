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
  , outputState :: Editor state -> Vty.Picture
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
  { getPicture :: Vty.Picture
  , isFinished :: Bool
  , nextAction :: Vty.Event -> EditorOutput
    -- TODO: Expand with basic events like saving and opening
  }

{- |
   The EditorAccess class runs the function (c -> c) on the 'c' component
   of the editor. Assuming 'c' is a component of the editor. Run should
   access the apropriate appropriate 'c' in the editor

-}
class Command c where
  run :: (c -> b) -> Editor a -> Editor b


type Action a = Vty.Event -> Editor a -> EditorOutput

{- | BindKey is used to store keybinds as a combination of some key
     and and a command. It is existential so that we cann easily
     specify a collection of keybinds without having to care
     to much about the type of the command.
-}

{- | A mapping of keys to keybindings
-}
type KeyBinds state = Action state
