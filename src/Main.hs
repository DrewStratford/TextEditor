module Main where

  {-
    Cursor now fully works!
  -}
    
import Control.Monad
import Graphics.Vty

import Editor.Editor
import Editor.TextDisplay
import Editor.EditorTypes
import Editor.Modes
import Data.EditorFunctionsIO

import Configuration

main :: IO ()
main = do
  vty <- createVty
  td <- createTextDisplay 0 normalMode "src/Main.hs"
  (height, width) <- displayBounds $ outputIface vty
  let ed = createEditorOutput $ editor td height width
  keyLoop vty ed
  shutdown vty

keyLoop vty editor = do
  update vty (getPicture editor)
  input <- nextEvent vty
  case input of
    EvKey (KChar 'c') [MCtrl] -> return ()
    _ -> keyLoop vty (nextAction editor input)

createVty = do
  cnfg <- standardIOConfig
  mkVty cnfg
