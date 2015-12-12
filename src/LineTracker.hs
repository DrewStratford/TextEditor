module LineTracker
    ( LineTracker
    , lineTracker
    , cursorUp
    , cursorDown
    , cursorLeft
    , cursorRight
    , newline
    , asSeqCoOrd
    , line
    , lineLength
    , col
    , increase
    , decrease
    ) where

import Prelude hiding (lines)
import Zip.Zip

data LineTracker = LineTracker
    {
      lines         :: Zip Int,
      offset :: Int,
      line          :: Int,
      col           :: Int
    } deriving Show 

lineTracker = LineTracker ( 0 `insert` zipper) 0 0 0

cursorUp cursor
         -- we do left because the when we are at the first line we'll still
         -- have one int int the left side of the zipper and atStart would return false
    | atStart $ left $ lines cursor = cursor
    | otherwise = nCurs { offset = off,col = nCol}
  where off = offset cursor - lineLength nCurs
        lineNum = line cursor - 1
        nCol = min (col nCurs) (lineLength nCurs - 1)
        nCurs = cursor {  line = lineNum, lines = left (lines cursor)}

cursorDown cursor
    | atEnd $ lines cursor = cursor
    | otherwise = nCurs { offset = off, col = nCol}
  where off = offset cursor + lineLength cursor
        lineNum = line cursor + 1
        nCurs = cursor { offset = off, line = lineNum, lines = right (lines cursor)}
        nCol
           | atEnd $ lines nCurs = min (col nCurs) (lineLength nCurs )
           | otherwise            = min (col nCurs) (lineLength nCurs - 1)

cursorLeft cursor = cursor { col = max (col cursor -1) 0 }
cursorRight cursor = cursor { col = min (col cursor +1) len }
  where len
           | atEnd $ lines cursor = lineLength cursor
           -- we minus one because we don't want it going passed the newline
           | otherwise = lineLength cursor - 1
              
newline :: LineTracker -> LineTracker
newline cursor = cursorDown cursor
                 { col = 0
                   -- Updates line size and inserts remainder in newline
                 , lines = left $ r `insert` toPoint (const l) (lines cursor)
                 }
  -- splits line based on cursor 
  where (l, r) = (col cursor, lineLength cursor - col cursor) 
        
increase cursor = cursor{ col = col cursor + 1, lines = toPoint (+1) $ lines cursor}
decrease cursor = cursor{ col = col cursor - 1, lines = toPoint (\x -> x- 1) $ lines cursor}

asSeqCoOrd :: LineTracker -> Int
asSeqCoOrd lTracker = offset lTracker + col lTracker


lineLength  = withPoint id 0 . lines

