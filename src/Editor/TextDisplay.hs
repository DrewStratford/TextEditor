module Editor.TextDisplay
       ( TextDisplay (..)
       , Mode (..)
       , createTextDisplay
       , textDisplay
       ) where

import qualified Data.Map as M
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Data.TextBuffer

data Mode = Insert | Normal | Command | Visual (Int, Int) deriving (Show, Eq)

data TextDisplay = TextDisplay
  { text     :: TextBuffer
  , topLine  :: Int
  , leftCol  :: Int
  , window   :: Window
  , getMode  :: Mode
  , marks    :: M.Map String (Int, Int)
  , colAlign :: Int
  }

textDisplay :: Window -> TextDisplay
textDisplay window = TextDisplay (fromStrings []) 0 0 window Normal M.empty 0

createTextDisplay :: FilePath -> IO TextDisplay
createTextDisplay filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  window <- initScr
  return $ TextDisplay textBuffer 0 0 window Normal M.empty 0
