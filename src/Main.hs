module Main where

import Control.Monad.State
    
import Data.TextBuffer
import TextMonad
import KeyInput


main :: IO ()
main = do
  td <- createTextDisplay "src/Main.hs"
  runTextM loop td

loop :: TextM ()
loop = do
  output
  input <- lift getKey
  output
  textDisplay <- get
  case mode textDisplay of
    Normal  -> normalKeys input
    Insert  -> insertKeys input
    Visual  -> visualKeys input
    Command -> loop

insertKeys :: Key -> TextM ()
insertKeys input = 
  case input of
    KeyEnter            -> do
                           toText newline
                           loop
    KeyEsc              -> do
                           setMode Normal
                           loop 
    KeyDelete           -> do
                           toText delete
                           loop
    KeyBackspace        -> do
                           toText backspace
                           loop
    KeyRight            -> do
                           moveColumn 1
                           loop
    KeyUp               -> do
                           moveLine (-1)
                           loop
    KeyDown             -> do
                           moveLine 1
                           loop
    KeyLeft             -> do
                           moveColumn (-1)
                           loop
    (KeyChar c  )       -> do
                           toText (`insert` c)
                           loop 
    _                   -> loop 

normalKeys input = 
  case input of
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
                           (l,c) <- getLineColumn
                           setMark (l,c) "start"
                           loop
    (KeyChar 'p')       -> do
                           insertClipBoard
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
    (KeyChar 'x')       -> do
                           cursor <- getLineColumn
                           setMark cursor "end"
                           cutToClipBoard
                           setMode Normal
                           loop
    (KeyChar 'y')       -> do
                           cursor <- getLineColumn
                           setMark cursor "end"
                           copyToClipBoard
                           setMode Normal
                           loop
    _                   -> loop

