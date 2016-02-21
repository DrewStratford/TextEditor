module Configuration where

import Commands

import Editor.Editor
import Editor.TextDisplay
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
    , BindKey KeyEsc       $ setGetMode Normal
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
    , BindKey (KeyChar 'i') $ modifyTextDisplay $ setGetMode Insert
    , BindKey (KeyChar 'v') $ \t -> let cursor = getLineColumn t
                                    in  (modifyTextDisplay $ setGetMode $ Visual cursor) t
    , BindKey (KeyChar 'p') $ insertClipBoard
    --, BindKey (KeyChar '$') $ \t -> let (_, c) = getLineColumn t in moveColumn 
    , BindKey (KeyChar '0') $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    , BindKey (KeyChar 'Q') $ modifyTextDisplay (setGetMode Command) 
    ]

visualKeys = makeKeyBinds
    [ BindKey (KeyChar '\ESC') $ setGetMode Normal
    , BindKey (KeyChar 'l')    $ moveColumn 1
    , BindKey (KeyChar 'k')    $ moveLine (-1)
    , BindKey (KeyChar 'j')    $ moveLine 1
    , BindKey (KeyChar 'h')    $ moveColumn (-1)
    , BindKey (KeyChar 'x')    $ modifyTextDisplay (setGetMode Normal) . cutToClipBoard
    , BindKey (KeyChar 'y')    $ modifyTextDisplay (setGetMode Normal) . copyToClipBoard
    ]
