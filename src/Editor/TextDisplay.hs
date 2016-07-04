{-# LANGUAGE ExistentialQuantification #-}
module Editor.TextDisplay
       ( TextDisplay (..)
       , createTextDisplay
       , textDisplay
       , setText
       , setTopLine
       , setLeftCol
       , setMode
       , setMarks           
       , setColAlign
       , modifyText
       , modifyTopLine
       , modifyLeftCol
--       , modifyGetMode
       , modifyMarks
       , modifyColAlign
       ) where

import qualified Data.Map as M

import Data.TextBuffer
import Editor.EditorTypes

-----------------------------------------------------------------------------------------------------
-- setters and modifiers

setText      insertee textDis = textDis{ text     = insertee }
setTopLine   insertee textDis = textDis{ topLine  = insertee }
setLeftCol   insertee textDis = textDis{ leftCol  = insertee }
setMode   state mode textDis = textDis{ getMode  = mode , state = state}
setMarks     insertee textDis = textDis{ marks    = insertee }
setColAlign  insertee textDis = textDis{ colAlign = insertee }

modifyText     f textDis = textDis{ text     = f $ text     textDis }
modifyTopLine  f textDis = textDis{ topLine  = f $ topLine  textDis }
modifyLeftCol  f textDis = textDis{ leftCol  = f $ leftCol  textDis }
--modifyGetMode  f textDis = textDis{ getMode  = f $ getMode  textDis }
modifyMarks    f textDis = textDis{ marks    = f $ marks    textDis }
modifyColAlign f textDis = textDis{ colAlign = f $ colAlign textDis }

textDisplay :: state -> Mode state -> TextDisplay state
textDisplay state mode = TextDisplay (fromStrings []) 0 0 state mode M.empty 0 "" 8

createTextDisplay ::  state -> Mode state -> FilePath -> IO (TextDisplay state)
createTextDisplay state mode filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  return $ TextDisplay
    textBuffer
    0 0
    state
    mode
    M.empty
    0
    filePath
    8
