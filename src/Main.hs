module Main where

  {-
    Cursor now fully works!
  -}
    
import System.IO.Unsafe(unsafeInterleaveIO)
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
  let ed = editor td height width
  kStrm <- keyStream vty
  let outStream = startStream kStrm ed
  eatOutputStream vty outStream
  shutdown vty

-- TODO: Replace with a streaming library
-- exits early with ctrl-c
keyStream :: Vty -> IO [Event]
keyStream vty = unsafeInterleaveIO $ do
  input <- nextEvent vty
  case input of
    EvKey (KChar 'c') [MCtrl] -> return []
    _ -> fmap (input:) (keyStream vty)

eatKeyStream :: Vty -> [Event] -> IO ()
eatKeyStream vty keys = case keys of
  []               -> return ()
  (EvKey (KChar 'Q') []: _)   -> return ()
  (k: ks)          -> update vty (picForImage $ string defAttr $ show k) >> eatKeyStream vty ks
  
eatOutputStream vty [] = return ()
eatOutputStream vty (o:os) = do
  let p = getPicture o
  update vty p
  eatOutputStream vty os

createVty = do
  cnfg <- standardIOConfig
  mkVty cnfg
