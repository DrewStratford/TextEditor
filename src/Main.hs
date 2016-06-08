module Main where
    
import KeyInput

import UI.HSCurses.Curses hiding ( Key(..))
import UI.HSCurses.CursesHelper hiding (getKey)

import Editor.Editor
import Editor.TextDisplay
import Editor.EditorTypes
import Editor.Modes
import Data.EditorFunctionsIO

import Configuration

main :: IO ()
main = do
  start
  --raw True
  td <- createTextDisplay normalMode "src/Main.hs"
  let ed = editor td
  iterateTill isFinished waitforKey ed
  endWin

iterateTill :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateTill pred f a =
  if pred a
     then return a
     else do a' <- f a
             iterateTill pred f a'

waitforKey editor = do
  output editor
  refresh
  input <- getKey
  let mode :: EditorMode
      mode    = getMode $ getTextDisplay editor 
      editor' = getKeyBinding input mode editor
  return editor'
