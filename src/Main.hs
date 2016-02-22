module Main where

    
import Data.Maybe
import KeyInput

import UI.HSCurses.Curses hiding (getKey, Key(..))
import UI.HSCurses.CursesHelper hiding (getKey, Key(..))


import Editor.Editor
import Editor.TextDisplay
import Data.EditorFunctions
import Data.EditorFunctionsIO

import Configuration

main :: IO ()
main = do
  start
  --raw True
  td <- createTextDisplay normalMode "src/Main.hs"
  let ed = editor td
  loop ed
  endWin

loop editor = do
  output editor
  refresh
  input <- getKey
  let mode = getMode $ getTextDisplay editor
  loop $ getBinding mode input editor
  {-
  case getMode $ getTextDisplay editor of
    Normal   -> loop $ (fromMaybe  id $ lookUpKey input normalKeys) editor
    Insert   -> loop $ (fromMaybe id $ lookUpKeyInsertMode input insertKeys) editor
    Visual _ -> loop $ (fromMaybe id $ lookUpKey input visualKeys) editor
    Command  -> return ()
  -}

-------------------------------------------------------------------
{-

insertKeys :: Key -> Editor -> Editor
insertKeys input = 
  case input of
    KeyEnter            -> toText newline 
    KeyEsc              -> modifyTextDisplay (setGetMode Normal) 
    KeyDelete           -> toText delete
                           
    KeyBackspace        -> toText backspace
    KeyRight            -> moveColumn 1
    KeyUp               -> modifyTextDisplay $ moveLine (-1)
    KeyDown             -> modifyTextDisplay $ moveLine 1
    KeyLeft             -> moveColumn (-1)
    (KeyChar c  )       -> toText (`insert` c)
    KeyTab              -> toText (`insert` '\t')
    _                   -> id

normalKeys input = 
  case input of
    (KeyChar 'l')       -> moveColumn 1
    (KeyChar 'k')       -> modifyTextDisplay $ moveLine (-1)
    (KeyChar 'j')       -> modifyTextDisplay $ moveLine 1
    (KeyChar 'h')       -> moveColumn (-1)
    (KeyChar 'i')       -> modifyTextDisplay $ setGetMode Insert
    (KeyChar 'v')       -> \t -> let cursor = getLineColumn t
                                 in  (modifyTextDisplay $ setGetMode $ Visual cursor) t
    (KeyChar 'p')       -> insertClipBoard
    (KeyChar '$')       -> id
    (KeyChar '0')       -> \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    (KeyChar 'Q')       -> modifyTextDisplay (setGetMode Command) 
    _                   -> id
                  
visualKeys input =
  case input of
    (KeyChar '\ESC')    -> modifyTextDisplay (setGetMode Normal)
    (KeyChar 'l')       -> moveColumn 1
    (KeyChar 'k')       -> modifyTextDisplay $ moveLine (-1)
    (KeyChar 'j')       -> modifyTextDisplay $ moveLine 1
    (KeyChar 'h')       -> moveColumn (-1)
    (KeyChar 'x')       -> modifyTextDisplay (setGetMode Normal) . cutToClipBoard
    (KeyChar 'y')       -> modifyTextDisplay (setGetMode Normal) . copyToClipBoard
    _                   -> id

-}
