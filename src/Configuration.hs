module Configuration
       ( insertMode
       , normalMode
       , visualMode
       , Editor.Modes.lookUpKey
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

  
instance Command Editor where
  run cmd = cmd 

instance Command TextDisplay where
  run =  modifyTextDisplay
  
insertKeys = makeKeyBinds
    [ BindKey KEnter [] $ toText newline 
    , BindKey KEsc   [] $ setGetMode $ EditorMode normalMode
    , BindKey KDel   [] $ toText delete
    , BindKey KBS    [] $ toText backspace
    , BindKey KRight [] $ moveColumn 1
    , BindKey KUp    [] $ moveLine (-1)
    , BindKey KDown  [] $ moveLine 1
    , BindKey KLeft  [] $ moveColumn (-1)
    ]

normalKeys = makeKeyBinds
    [ BindKey (KChar 'l') [] $ moveColumn 1
    , BindKey (KChar 'k') [] $ moveLine (-1)
    , BindKey (KChar 'j') [] $ moveLine 1
    , BindKey (KChar 'h') [] $ moveColumn (-1)
    , BindKey (KChar 'i') [] $ setGetMode $ EditorMode insertMode
    , BindKey (KChar 'p') []  insertClipBoard
    , BindKey (KChar '0') [] $ \t -> let (_, c) = getLineColumn t in moveColumn (-c) t
    , BindKey (KChar ';') [] $ setGetMode $ EditorMode commandMode
    , BindKey (KChar 'Q') []  endSession
    , BindKey (KChar 's') [MCtrl]  (\x -> setPendingIO (saveFile x) x)
    ]

commandKeys = makeKeyBinds
    [ BindKey KEnter  [] $ setGetMode $ EditorMode normalMode
    , BindKey KEsc    [] $ setGetMode $ EditorMode normalMode
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

-- | stores the amount of times command should be repeated as an int
newtype NormalMode  = NormalMode Int
newtype InsertMode  = InsertMode ()
newtype CommandMode = CommandMode TextBuffer
newtype VisualMode  = VisualMode (Int, Int)

insertMode  = InsertMode ()
normalMode  = NormalMode 0
visualMode  = undefined
commandMode = CommandMode $ fromStrings []

instance Mode InsertMode where
  outputState _ _ = return ()
  keyBindings _   = insertKeys
  updateState _ a = a
  getCommand key mods state = command
    -- if the key is a keyChar we "type" it otherwise check for keybind
    where command = case key of
            (KChar c) -> toText (`insert` c)
            _         -> keyToCommand key mods state

{-
  TODO: Consider whether it would be better to store commands as an string and incrementally
        parsing it. This could use the existing trie data structure, that used to parse
        key codes.
-}
instance Mode NormalMode where
  outputState _ _ = return ()

  keyBindings _   = normalKeys

  updateState key (NormalMode i) = case key of
    (KChar c) -> let num         = readMaybe [c]
                     shiftedLeft = i * 10
                 in maybe (NormalMode 0) (\x -> NormalMode $ shiftedLeft + x ) num
    _         -> NormalMode 0

  getCommand key mods state@(NormalMode reps) = withRepetition
    where command        = keyToCommand key mods state
          withRepetition = composeN (max reps 1) command 

instance Mode CommandMode where
  outputState (scrnHeight,scrnWidth) (CommandMode textBuf) = return ()
  keyBindings _ = commandKeys
  updateState key (CommandMode a) = case key of
    KChar c -> CommandMode $ a `insert` c
    _       -> CommandMode a

  getCommand = keyToCommand

------------------------------------------------------------
-- should be somewhere else
composeN :: Int -> (a -> a) -> a -> a 
composeN i f = foldl (.) id $ replicate i f

keyToCommand key mods state = fromMaybe id $ lookUpKey key mods state

