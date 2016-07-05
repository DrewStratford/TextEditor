{-# LANGUAGE ExistentialQuantification #-}
module Editor.TextDisplay
       ( TextDisplay (..)
       , createTextDisplay
       , textDisplay
       , setText
       , setTopLine
       , setLeftCol
       , setMarks           
       , setColAlign
       , modifyText
       , modifyTopLine
       , modifyLeftCol
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
setMarks     insertee textDis = textDis{ marks    = insertee }
setColAlign  insertee textDis = textDis{ colAlign = insertee }

modifyText     f textDis = textDis{ text     = f $ text     textDis }
modifyTopLine  f textDis = textDis{ topLine  = f $ topLine  textDis }
modifyLeftCol  f textDis = textDis{ leftCol  = f $ leftCol  textDis }
modifyMarks    f textDis = textDis{ marks    = f $ marks    textDis }
modifyColAlign f textDis = textDis{ colAlign = f $ colAlign textDis }

textDisplay :: TextDisplay
textDisplay = TextDisplay (fromStrings []) 0 0 M.empty 0 "" 8

createTextDisplay :: FilePath -> IO TextDisplay
createTextDisplay filePath = do
  file <- readFile filePath
  let textBuffer = fromStrings $ lines file
  return $ TextDisplay
    textBuffer
    0 0
    M.empty
    0
    filePath
    8

{-
instance EditorAccess (TextDisplay a) where
    get = getTextDisplay
    -}
