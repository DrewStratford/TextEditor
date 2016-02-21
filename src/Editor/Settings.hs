module Editor.Settings
       ( Settings (..)
       , defaultSettings
       , setTabs
       , setTabSize
       , setLineNums
       )where

import Data.Map

data Settings = Settings
    { tabs :: Bool
    , tabSize :: Int
    , lineNums :: Bool
    } deriving (Show, Eq)

setTabs i set = set { tabs = i }
setTabSize i set = set { tabSize = i }
setLineNums b set = set { lineNums = b }

defaultSettings = Settings
   { tabs = False
   , tabSize = 4
   , lineNums = True
   }
