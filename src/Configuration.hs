module Configuration
       ( insertMode
       , normalMode
       , visualMode
       , Editor.Modes.lookUpKey
       ) where

import Data.Maybe
import Text.Read

import UI.HSCurses.Curses hiding (Key (..))

import Commands

import Editor.Editor
import Editor.EditorTypes
import Editor.TextDisplay
import Editor.Modes

import Data.TextBuffer
import Data.EditorFunctions
import Data.EditorFunctionsIO

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
    , BindKey (KeyChar ';') $ setGetMode $ EditorMode commandMode
    , BindKey (KeyChar 'Q') $ endSession
    ]

commandKeys = makeKeyBinds
    [ BindKey KeyEnter     $ setGetMode $ EditorMode normalMode
    , BindKey KeyEsc       $ setGetMode $ EditorMode normalMode
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
newtype NormalMode = NormalMode Int
newtype InsertMode = InsertMode ()
newtype CommandMode = CommandMode TextBuffer
newtype VisualMode = VisualMode (Int, Int)

insertMode = InsertMode ()
normalMode = NormalMode 0
visualMode = undefined
commandMode = CommandMode $ fromStrings []

instance Mode InsertMode where
  outputState _ _ = return ()
  keyBindings _ = insertKeys
  updateState _ a = a
  getCommand key state = fromMaybe return command
    -- if the key is a keyChar we "type" it otherwise check for keybind
    where command = case key of
            (KeyChar c) -> Just $ \ed -> Just $ toText (`insert` c) ed
            _ -> lookUpKey key state

instance Mode NormalMode where
  outputState _ _ = return ()
  keyBindings _ = normalKeys
  updateState key (NormalMode i) = case key of
    (KeyChar c) -> let num  :: Maybe Int
                       num = readMaybe [c]
                       shiftedLeft = i * 10
                   in maybe (NormalMode 0) (\x -> NormalMode $ shiftedLeft + x ) num
    _           -> NormalMode 0
  getCommand key state@(NormalMode reps) = fromMaybe return withRepetition
    where command = lookUpKey key state
          withRepetition :: Maybe (Editor -> Maybe Editor)
          withRepetition = composeN reps <$> command 

instance Mode CommandMode where
  outputState (scrnHeight,scrnWidth) (CommandMode textBuf) = do
    move (scrnHeight - 1) 0
    drawSection textBuf scrnWidth
  keyBindings _ = commandKeys
  updateState key (CommandMode a) = case key of
    KeyChar c -> CommandMode $ a `insert` c
    _         -> CommandMode a

  getCommand key state = fromMaybe return (lookUpKey key state)

-- should be somewhere else
composeN :: Int -> (a -> Maybe a) -> a -> Maybe a 
composeN 0 f a = f a
composeN i f a =  composeN (i - 1) f a >>= \x ->  f x
