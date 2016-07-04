module Configuration
       ( insertMode
       , normalMode
       ) where

import Data.Maybe
import Text.Read

import Commands

import Editor.Editor
import Editor.EditorTypes
import Editor.TextDisplay
import Editor.Modes

import Data.TextBuffer
import Data.EditorFunctions
import Data.EditorFunctionsIO

import Graphics.Vty(Key(..), Modifier(..))

  

{-
instance Command (Editor a) where
  run cmd = cmd 

instance Command (TextDisplay a) where
  run =  modifyTextDisplay
-}
  
insertKeys = makeKeyBinds
    [ bindKey KEnter [] $ toText newline 
    , bindKey KEsc   [] $ modifyTextDisplay $ setMode 0 normalMode
    , bindKey KDel   [] $ toText delete
    , bindKey KBS    [] $ toText backspace
    , bindKey KRight [] $ moveColumn 1
    , bindKey KUp    [] $ modifyTextDisplay $ moveLine (-1)
    , bindKey KDown  [] $ modifyTextDisplay $ moveLine 1
    , bindKey KLeft  [] $ moveColumn (-1)
    ]

normalKeys = makeKeyBinds
    [ bindKey (KChar 'l') [] $ moveColumn 1
    , bindKey (KChar 'k') [] $ modifyTextDisplay $ moveLine (-1)
    , bindKey (KChar 'j') [] $ modifyTextDisplay $ moveLine 1
    , bindKey (KChar 'h') [] $ moveColumn (-1)
    , bindKey (KChar 'i') [] $ modifyTextDisplay $ setMode () insertMode
    , bindKey (KChar 'p') []  insertClipBoard
    , bindKey (KChar '0') [] $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    --, bindKey (KChar ';') [] $ modifyTextDisplay $ setMode undefined undefined
    --, bindKey (KChar 'Q') []  endSession
    ]

commandKeys = makeKeyBinds
    [ bindKey KEnter  [] $ modifyTextDisplay $ setMode undefined undefined
    , bindKey KEsc    [] $ modifyTextDisplay $ setMode undefined undefined
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
insertMode :: Mode a
insertMode = Mode insertKeys output 
normalMode :: Mode Int
normalMode = Mode normalKeys output 



output editor = EditorOutput (updateImage editor) False
------------------------------------------------------------
-- should be somewhere else
composeN :: Int -> (a -> a) -> a -> a 
composeN i f = foldl (.) id $ replicate i f


