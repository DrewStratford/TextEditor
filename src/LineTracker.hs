module LineTracker
    ( LineTracker
    , lineTracker
    , cursorUp
    , cursorDown
    , cursorLeft
    , cursorRight
    , newline
    , asSeqCoOrd
    , lineLength
    , line
    , col
    , increase
    , decrease
    ) where

import Prelude hiding (lines)
import Data.PrefixTree
    
data LineTracker = LineTracker
    {
      lines         :: PrefixTree,
      offset        :: Int,
      lineLength    :: Int,
      line          :: Int,
      col           :: Int
    } deriving Show 

lineTracker :: LineTracker
lineTracker = LineTracker (insertAt 0 0 empty) 0 0 0 0 

changeLine :: Int -> LineTracker -> LineTracker
changeLine i cursor
    | i < 0  = cursor
    | i >= size (lines cursor) = cursor
    | otherwise =
        cursor{ offset = off
              , col    = newCol
              , line   = i
              , lineLength = newLen
              , lines = uLines
              }
  where off    = prefixAt i uLines
        newCol = min (max 0 $ newLen - 1)  (col cursor)
        newLen = valueAt i $ lines cursor
        -- update the line size at prev line
        uLines = updateAt (line cursor) (const $ lineLength cursor) (lines cursor)
                 
cursorUp cursor = changeLine l cursor
  where l = line cursor - 1

cursorDown cursor = changeLine l cursor
  where l = line cursor + 1

cursorLeft cursor = cursor { col = max (col cursor -1) 0 }
cursorRight cursor = cursor { col = min (col cursor +1) len }
  where len
          | size (lines cursor) - 1 == line cursor = lineLength cursor
          | otherwise = lineLength cursor - 1
              
newline :: LineTracker -> LineTracker
newline cursor =  cursor {
                    lines = uLines,
                    col   = 0,
                    line  = line cursor + 1,
                    offset = off + split,
                    lineLength = newLineLen
                    }
  where uLines     =
            -- could probably clean this up
            insertAt (line cursor) newLineLen
                         $ updateAt (line cursor) (const split) (lines cursor)
        split      = lineLength cursor - newLineLen
        newLineLen = lineLength cursor - col cursor
        off        = prefixAt (line cursor) $ lines cursor
                  
increase cursor = cursor {
                    lineLength = max 0 $ lineLength cursor + 1,
                    col = col cursor + 1
                    }
decrease cursor = cursor {
                    lineLength = lineLength cursor + 1,
                    col = col cursor - 1
                    }

asSeqCoOrd :: LineTracker -> Int
asSeqCoOrd lTracker = offset lTracker + col lTracker



