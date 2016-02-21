module Editor.TextDisplay
       ( TextDisplay (..)
       , Mode (..)
       , createTextDisplay
       , textDisplay
       , setText
       , setTopLine
       , setLeftCol
       , setGetMode
       , setMarks           
       , setColAlign
       , modifyText
       , modifyTopLine
       , modifyLeftCol
       , modifyGetMode
       , modifyMarks
       , modifyColAlign
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
  , getMode  :: Mode
  , marks    :: M.Map String (Int, Int)
  , colAlign :: Int
  }




-----------------------------------------------------------------------------------------------------
-- setters and modifiers

setText      insertee textDis= textDis{ text     = insertee }
setTopLine   insertee textDis= textDis{ topLine  = insertee }
setLeftCol   insertee textDis= textDis{ leftCol  = insertee }
setGetMode   insertee textDis= textDis{ getMode  = insertee }
setMarks     insertee textDis= textDis{ marks    = insertee }
setColAlign  insertee textDis= textDis{ colAlign = insertee }

modifyText     f textDis= textDis{ text     = f $ text     textDis }
modifyTopLine  f textDis= textDis{ topLine  = f $ topLine  textDis }
modifyLeftCol  f textDis= textDis{ leftCol  = f $ leftCol  textDis }
modifyGetMode  f textDis= textDis{ getMode  = f $ getMode  textDis }
modifyMarks    f textDis= textDis{ marks    = f $ marks    textDis }
modifyColAlign f textDis= textDis{ colAlign = f $ colAlign textDis }

textDisplay :: TextDisplay
textDisplay = TextDisplay (fromStrings []) 0 0 Normal M.empty 0

createTextDisplay :: FilePath -> IO TextDisplay
createTextDisplay filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  return $ TextDisplay textBuffer 0 0 Normal M.empty 0
