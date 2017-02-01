{-# LANGUAGE OverloadedStrings #-}
module Main where

  {-
    Cursor now fully works!
  -}
    
import Control.Monad
import Control.Monad
import Data.IORef

import Graphics.Vty

import Control.TextMonad

main :: IO ()
main = do
  vty <- createVty
  file <- readFile "/home/drew/Documents/Writing/Writing_Ideas-July_11.txt"
  ref <- newIORef mempty
  let state = emptyTextState vty "scratch" ref
  run (start file) state
  shutdown vty

start :: String -> TextT IO ()
start file = do
  insertString file
  setColumn 0
  drawText
  flush
  loop
  
loop :: TextT IO ()
loop = do
  key <- getKey
  unless (key == EvKey (KChar 'c') [MCtrl]) $ do
    case key of
      EvKey (KChar c) [] -> insertChar c
      EvKey KBS []       -> deleteAmount 1
      EvKey KEnter []    -> insertText "\n"
      EvKey (KChar 'o') [MCtrl] -> open "test"
      EvKey (KChar 'x') [MCtrl] -> close
      --EvKey (KChar 'q') [MCtrl] -> askQuestion "enter something" >>= insertString
        
      EvKey KEnd [] -> moveToEOL
      EvKey KHome [] -> moveToSOL
      EvKey KLeft [] -> moveColumn (-1)
      EvKey KRight [] -> moveColumn 1

      EvKey KLeft [MShift] -> previous 
      EvKey KRight [MShift] -> next 
      -- prints the combo onto the document
      other                -> insertString (show other)
    drawText
    flush
    loop



  
  
createVty = do
  cnfg <- standardIOConfig
  mkVty cnfg
