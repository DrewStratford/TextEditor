module Configuration
       ( insertMode
       , normalMode
       , visualMode
       , Editor.Modes.lookUpKey
       ) where

import qualified Data.Map as M
import Data.Maybe

import Commands

import Editor.Editor
import Editor.EditorTypes
import Editor.TextDisplay
import Editor.Modes

import Data.TextBuffer
import Data.EditorFunctions
import Data.EditorFunctionsIO

import Editor.Settings

import KeyInput

-- this is to end the session (A bit hacky)
type END = ()

endSession = const ()

instance Command () where
  run a b= Nothing
  
instance Command Editor where
  run cmd editor= Just $ cmd editor

instance Command TextDisplay where
  run cmd editor= Just $ modifyTextDisplay cmd editor
  
insertKeys = makeKeyBinds
    [ BindKey KeyEnter     $ toText newline 
    , BindKey KeyEsc       $ setGetMode $ EditorMode normalMode
    , BindKey KeyDelete    $ toText delete
    , BindKey KeyBackspace $ toText backspace
    , BindKey KeyRight     $ moveColumn 1
    , BindKey KeyUp        $ moveLine (-1)
    , BindKey KeyDown      $ moveLine 1
    , BindKey KeyLeft      $ moveColumn (-1)
    , BindKey KeyTab       $ toText (`insert` '\t')
    ]

normalKeys = makeKeyBinds
    [ BindKey (KeyChar 'l') $ moveColumn 1
    , BindKey (KeyChar 'k') $ moveLine (-1)
    , BindKey (KeyChar 'j') $ moveLine 1
    , BindKey (KeyChar 'h') $ moveColumn (-1)
    , BindKey (KeyChar 'i') $ setGetMode $ EditorMode insertMode
 --   , BindKey (KeyChar 'v') $ \t -> let cursor = getLineColumn t
  --                                  in  (modifyTextDisplay $ setGetMode $ visualMode cursor) t
    , BindKey (KeyChar 'p') $ insertClipBoard
    --, BindKey (KeyChar '$') $ \t -> let (_, c) = getLineColumn t in moveColumn 
    , BindKey (KeyChar '0') $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    , BindKey (KeyChar 'Q') $ endSession
    ]

{-
visualKeys = makeKeyBinds
    [ BindKey (KeyChar '\ESC') $ setGetMode normalMode
    , BindKey (KeyChar 'l')    $ moveColumn 1
    , BindKey (KeyChar 'k')    $ moveLine (-1)
    , BindKey (KeyChar 'j')    $ moveLine 1
    , BindKey (KeyChar 'h')    $ moveColumn (-1)
    , BindKey (KeyChar 'x')    $ modifyTextDisplay (setGetMode normalMode) . cutToClipBoard
    , BindKey (KeyChar 'y')    $ modifyTextDisplay (setGetMode normalMode) . copyToClipBoard
    ]
-}
------------------------------------------------------------------------------------------------------
    -- modes

newtype NormalMode = NormalMode Int
newtype InsertMode = InsertMode ()
newtype VisualMode = VisualMode (Int, Int)

insertMode :: InsertMode
insertMode = InsertMode ()
normalMode = NormalMode 1
visualMode = undefined

instance Mode InsertMode where
  keyBindings _ = insertKeys
  updateState _ a = a
  getCommand key state = fromMaybe return command

    where command = case key of
            (KeyChar c) -> Just $ \ed -> Just $ toText (`insert` c) ed
            _ -> lookUpKey key state

instance Mode NormalMode where
  keyBindings _ = normalKeys
  updateState key a = a 
  getCommand key state = fromMaybe return (command)
    where command = lookUpKey key state


