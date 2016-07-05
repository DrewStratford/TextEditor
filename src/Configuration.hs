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

import Graphics.Vty(Event(..), Key(..), Modifier(..))

  

{-
instance Command (Editor a) where
  run cmd = cmd 

instance Command (TextDisplay a) where
  run =  modifyTextDisplay
-}
  
insertKeys event editor = 
  let i = undefined
  in case event of
     EvKey KEnter [] -> createEditorOutput $ toText newline editor
     EvKey KEsc   [] -> createEditorOutput $ modifyTextDisplay (setMode 0 normalMode) editor
     EvKey KDel   [] -> createEditorOutput $ toText delete editor
     EvKey KBS    [] -> createEditorOutput $ toText backspace editor
     EvKey KRight [] -> createEditorOutput $ moveColumn 1 editor
     EvKey KUp    [] -> createEditorOutput $ modifyTextDisplay (moveLine (-1)) editor
     EvKey KDown  [] -> createEditorOutput $ modifyTextDisplay (moveLine 1) editor
     EvKey KLeft  [] -> createEditorOutput $ moveColumn (-1) editor
     _               -> createEditorOutput editor
    

normalKeys event editor = case event of
    EvKey (KChar 'l') [] -> createEditorOutput $ moveColumn 1 editor
    EvKey (KChar 'k') [] -> createEditorOutput $ modifyTextDisplay (moveLine (-1)) editor
    EvKey (KChar 'j') [] -> createEditorOutput $ modifyTextDisplay (moveLine 1) editor
    EvKey (KChar 'h') [] -> createEditorOutput $ moveColumn (-1) editor
    EvKey (KChar 'i') [] -> createEditorOutput $ modifyTextDisplay (setMode () insertMode) editor
    EvKey (KChar 'p') [] -> createEditorOutput $ insertClipBoard editor
    --EvKey (KChar '0') [] -> createEditorOutput $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    _                    -> createEditorOutput editor

------------------------------------------------------------------------------------------------------
    -- modes
insertMode :: Mode a
insertMode = Mode insertKeys updateImage 
normalMode :: Mode Int
normalMode = Mode normalKeys updateImage 



------------------------------------------------------------
-- should be somewhere else
composeN :: Int -> (a -> a) -> a -> a 
composeN i f = foldl (.) id $ replicate i f


