module Configuration
       ( insertMode
       , normalMode
       , visualMode
       , Editor.Modes.getBinding
       ) where

import qualified Data.Map as M

import Commands

import Editor.Editor
import Editor.TextDisplay
import Editor.Modes

import Data.TextBuffer
import Data.EditorFunctions
import Data.EditorFunctionsIO

import Editor.Settings

import KeyInput

instance Command Editor where
  run = id

instance Command TextDisplay where
  run = modifyTextDisplay
  
insertKeys = makeKeyBinds
    [ BindKey KeyEnter     $ toText newline 
    , BindKey KeyEsc       $ setGetMode normalMode
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
    , BindKey (KeyChar 'i') $ setGetMode insertMode
    , BindKey (KeyChar 'v') $ \t -> let cursor = getLineColumn t
                                    in  (modifyTextDisplay $ setGetMode $ visualMode cursor) t
    , BindKey (KeyChar 'p') $ insertClipBoard
    --, BindKey (KeyChar '$') $ \t -> let (_, c) = getLineColumn t in moveColumn 
    , BindKey (KeyChar '0') $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    --, BindKey (KeyChar 'Q') $ (setGetMode id) 
    ]

visualKeys = makeKeyBinds
    [ BindKey (KeyChar '\ESC') $ setGetMode normalMode
    , BindKey (KeyChar 'l')    $ moveColumn 1
    , BindKey (KeyChar 'k')    $ moveLine (-1)
    , BindKey (KeyChar 'j')    $ moveLine 1
    , BindKey (KeyChar 'h')    $ moveColumn (-1)
    , BindKey (KeyChar 'x')    $ modifyTextDisplay (setGetMode normalMode) . cutToClipBoard
    , BindKey (KeyChar 'y')    $ modifyTextDisplay (setGetMode normalMode) . copyToClipBoard
    ]

insertMode = Mode insertKeys lookUpKeyInsertMode Nothing
normalMode = Mode normalKeys defaultLookUp Nothing
visualMode cursor = Mode visualKeys defaultLookUp (Just cursor)

lookUpKeyInsertMode :: Key -> KeyBinds -> Maybe (Editor -> Editor)
lookUpKeyInsertMode key bindings = case defaultLookUp key bindings of
  Nothing   -> case key of
    (KeyChar c) -> Just $ toText (`insert` c)
    _           -> Nothing
  something -> something
