module Main where
    
import KeyInput

import UI.HSCurses.Curses hiding ( Key(..))
import UI.HSCurses.CursesHelper hiding (getKey)

import Editor.Editor
import Editor.TextDisplay
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

  -- check for end or error
  case getBinding mode input editor of
    Nothing  -> return ()
    (Just a) -> loop a
