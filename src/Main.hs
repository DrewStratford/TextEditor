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
  loop ed
  endWin

loop editor = do
  output editor
  refresh
  input <- getKey
  let mode :: EditorMode
      mode = getMode $ getTextDisplay editor

  -- check for end or error
  case getKeyBinding input mode editor of
    Nothing  -> return ()
    (Just a) -> loop a
