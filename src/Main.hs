{-# LANGUAGE OverloadedStrings #-}
module Main where

  {-
    Cursor now fully works!
  -}
    
import Control.Monad
import Control.Monad
import Data.IORef
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Graphics.Vty

import Control.TextMonad

main :: IO ()
main = do
  vty <- createVty
  file <- T.readFile "/home/drew/Documents/Writing/Writing_Ideas-July_11.txt"
  --file <- T.readFile "war_and_peace.txt" -- ~3mb good for speed testing
  --file <- T.readFile "big.txt" -- ~9mb good for speed testing
  --file <- T.readFile "bigger.txt" -- ~29mb good for speed testing
  ref <- newIORef mempty
  let state = emptyTextState vty "scratch" ref
  run (start file) state
  shutdown vty

start :: T.Text -> TextT IO ()
start file = do
  insertText file
  setPoint 0
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
      EvKey (KChar 'o') [MCtrl] -> open "(*new*)"
      EvKey (KChar 's') [MCtrl] -> saveFile
      EvKey (KChar 'x') [MCtrl] -> close
      --EvKey (KChar 'q') [MCtrl] -> askQuestion "enter something" >>= insertString
        
      EvKey KPageDown [] -> moveLine 45
      EvKey KPageUp [] -> moveLine (-45)
      EvKey KEnd [] -> moveToEOL
      EvKey KHome [] -> moveToSOL
      EvKey KLeft [] -> moveLeft
      EvKey KRight [] -> moveRight
      EvKey KDown [] -> moveLine 1
      EvKey KUp [] -> moveLine (-1)

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
