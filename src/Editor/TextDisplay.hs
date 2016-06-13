{-# LANGUAGE ExistentialQuantification #-}
module Editor.TextDisplay
       ( TextDisplay (..)
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
setGetMode   insertee textDis = textDis{ getMode  = insertee }
setMarks     insertee textDis = textDis{ marks    = insertee }
setColAlign  insertee textDis = textDis{ colAlign = insertee }

modifyText     f textDis = textDis{ text     = f $ text     textDis }
modifyTopLine  f textDis = textDis{ topLine  = f $ topLine  textDis }
modifyLeftCol  f textDis = textDis{ leftCol  = f $ leftCol  textDis }
--modifyGetMode  f textDis = textDis{ getMode  = f $ getMode  textDis }
modifyMarks    f textDis = textDis{ marks    = f $ marks    textDis }
modifyColAlign f textDis = textDis{ colAlign = f $ colAlign textDis }

textDisplay :: Mode mode => mode -> TextDisplay
textDisplay mode = TextDisplay (fromStrings []) 0 0 (EditorMode mode) M.empty 0 "" defVisualElement 8

createTextDisplay ::  Mode mode => mode  -> FilePath -> IO TextDisplay
createTextDisplay mode filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  return $ TextDisplay
    textBuffer
    0 0
    (EditorMode mode)
    M.empty
    0
    filePath
    defVisualElement
    8
