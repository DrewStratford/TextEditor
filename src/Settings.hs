module Settings
       ( Settings (..)
       , defaultSettings
       )where

import Data.Map

data Settings = Settings
    { tabs :: Bool
    , tabSize :: Int
    , lineNums :: Bool
    } deriving (Show, Eq)

defaultSettings = Settings
   { tabs = False
   , tabSize = 4
   , lineNums = True
   }
