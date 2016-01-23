module Main where

import Control.Monad.State
    
    
import Data.TextBuffer
import TextMonad

main :: IO ()
main = do
  td <- createTextDisplay "src/Main.hs"
  runTextM loop td

loop :: TextM ()
loop = do
  --wclear window 
  output
  input <- getInput
  textDisplay <- get
  case mode textDisplay of
    Normal  -> normalKeys input
    Insert  -> insertKeys input
    Visual  -> loop
    Command -> loop

insertKeys :: Key -> TextM ()
insertKeys input = 
  case input of
    (KeyChar '\ESC')    -> do
                           setMode Normal
                           loop 
    (KeyChar '\DEL')    -> do
                           toText backspace
                           loop
    (KeyChar c  )       -> do
                           toText (`insert` c)
                           loop 
    _                   -> loop 

normalKeys input = 
  case input of
    (KeyChar '\DEL')    -> do
                           toText backspace
                           loop
    (KeyChar 'l')       -> do
                           moveColumn 1
                           loop
    (KeyChar 'k')       -> do
                           moveLine (-1)
                           loop
    (KeyChar 'j')       -> do
                           moveLine 1
                           loop
    (KeyChar 'h')       -> do
                           moveColumn (-1)
                           loop
    (KeyChar 'i')       -> do
                           setMode Insert
                           loop
    (KeyChar 'v')       -> do
                           setMode Visual
                           loop
    (KeyChar 'Q')         -> return ()
    _                     -> loop 
                  
visualKeys input =
  case input of
    (KeyChar '\ESC')    -> do
                           setMode Normal
                           loop 
    (KeyChar 'l')       -> do
                           moveColumn 1
                           loop
    (KeyChar 'k')       -> do
                           moveLine (-1)
                           loop
    (KeyChar 'j')       -> do
                           moveLine 1
                           loop
    (KeyChar 'h')       -> do
                           moveColumn (-1)
                           loop
    _                   -> loop

