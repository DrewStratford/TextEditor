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
  
  
insertKeys event editor = 
  let state = undefined
  in case event of
     EvKey (KChar c) [] -> createEditorOutput $ toText (`insert` c) editor
     EvKey KEnter [] -> createEditorOutput $ toText newline editor
     EvKey KEsc   [] -> createEditorOutput $ setMode 0 normalMode editor
     EvKey KDel   [] -> createEditorOutput $ toText delete editor
     EvKey KBS    [] -> createEditorOutput $ toText backspace editor
     EvKey KRight [] -> createEditorOutput $ moveColumn 1 editor
     EvKey KUp    [] -> createEditorOutput $ modifyTextDisplay (moveLine (-1)) editor
     EvKey KDown  [] -> createEditorOutput $ modifyTextDisplay (moveLine 1) editor
     EvKey KLeft  [] -> createEditorOutput $ moveColumn (-1) editor
     _               -> createEditorOutput editor
    

normalKeys :: KeyBinds Int
normalKeys event ed =
  let moveAmount = max 1 (getState ed)
      editor     = increment event ed
  in case event of
     EvKey (KChar 'l') [] -> createEditorOutput $ moveColumn moveAmount editor
     EvKey (KChar 'k') [] -> createEditorOutput $ modifyTextDisplay (moveLine (- moveAmount)) editor
     EvKey (KChar 'j') [] -> createEditorOutput $ modifyTextDisplay (moveLine moveAmount) editor
     EvKey (KChar 'h') [] -> createEditorOutput $ moveColumn (-moveAmount) editor
     EvKey (KChar 'i') [] -> createEditorOutput $ setMode () insertMode editor
     EvKey (KChar 'p') [] -> createEditorOutput $ insertClipBoard editor
     --EvKey (KChar '0') [] -> createEditorOutput $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
     _                    -> createEditorOutput editor


-- | increments the editors "count" if it number is pressed
increment :: Event -> Editor Int -> Editor Int
increment (EvKey (KChar c) []) editor = 
  let digit = readMaybe [c] :: Maybe Int
  in case digit of 
      Nothing  -> setState 0 editor
      (Just i) -> modifyState (\s -> s * 10 + i) editor
increment _ editor = setState 0 editor

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


