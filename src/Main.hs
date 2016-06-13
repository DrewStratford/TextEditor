module Main where

  {-
    This is a test!
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
  td <- createTextDisplay normalMode "src/Main.hs"
  let ed = editor td
  waitforKey vty ed
  shutdown vty


waitforKey :: Vty -> Editor -> IO ()
waitforKey vty editor = do
  getPendingIO editor
  (height, width) <- displayBounds $ outputIface vty
  let editor'  = updateImage height width vty editor
      editor'' = setPendingIO (return ()) editor
      visualElem :: VisualElement
      visualElem = getImage $ getTextDisplay editor'

  update vty (picture visualElem) 
  input <- nextEvent vty


  case input of
    EvMouse{} -> waitforKey vty editor''
    EvResize{} -> waitforKey vty editor''
    EvKey key modifiers -> do 
                 let mode :: EditorMode
                     mode      = getMode $ getTextDisplay editor''
                     editor''' = getKeyBinding key modifiers mode editor''
                 unless (isFinished editor'') $ waitforKey vty editor'''

createVty = do
  cnfg <- standardIOConfig
  mkVty cnfg
