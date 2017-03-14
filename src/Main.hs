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
import System.Environment

import Graphics.Vty

import Control.TextMonad

main :: IO ()
main = do
  vty <- createVty
  args <- getArgs
  file <- if length args > 0
             then T.readFile $ args !! 0 
             else return ""
  ref <- newIORef mempty
  let name = if length args > 0 then args !! 0 else "scratch"
      state = emptyTextState vty name ref
  -- find screen dimensions
  (width, height) <- displayBounds $ outputIface vty
  run (start file width height) state
  shutdown vty

start :: T.Text -> Int -> Int -> TextT IO ()
start file width height = do
  insertText file
  point .= 0
  screenHeight .= height
  screenWidth  .= width
  drawText
  flush
  loop
  
loop :: TextT IO ()
loop = do
  key <- getKey
  unless (key == EvKey (KChar 'c') [MCtrl]) $ do
    case key of
      EvKey (KChar c) [] -> insertChar c
      EvKey KBS []       -> backspaceAmount 1
      EvKey KDel []      -> deleteAmount 1
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

      EvResize width height -> do
        screenHeight .= height
        screenWidth .= width
        
      -- prints the combo onto the document
      other                -> insertString (show other)
    drawText
    flush
    loop



  
  
createVty = do
  cnfg <- standardIOConfig
  mkVty cnfg
