module LineTracker
    ( LineTracker
    , lineTracker
    , up
    , down
    , newline
    , asSeqCoOrd
    , line
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

up cursor = nCurs { offset = off,col = min (col nCurs) (lineLength nCurs)}
  where off = offset cursor - lineLength nCurs
        nCurs = cursor {  line = lineNum, lines = left (lines cursor)}
        lineNum
            | atStart $ lines cursor = 0
            | otherwise =  line cursor - 1

down cursor = nCurs { offset = off, col = min (col nCurs) (lineLength nCurs)}
  where off = offset cursor + lineLength cursor
        nCurs = cursor { offset = off, line = lineNum, lines = right (lines cursor)}
        lineNum
            | atStart $ lines cursor = line cursor
            | otherwise =  line cursor + 1

newline :: LineTracker -> LineTracker
newline cursor = down cursor
                 { col = 0
                   -- updates line size and inserts remainder in newline
                 , lines = left $ r `insert` toPoint (const l) (lines cursor)
                 }
  -- splits line based on cursor 
  where (l, r) = (col cursor, lineLength cursor - col cursor) 
        
increase cursor = cursor{ col = col cursor + 1, lines = toPoint (+1) $ lines cursor}
decrease cursor = cursor{ col = col cursor - 1, lines = toPoint (\x -> x- 1) $ lines cursor}

asSeqCoOrd :: LineTracker -> Int
asSeqCoOrd lTracker = offset lTracker + col lTracker


lineLength  = withPoint id 0 . lines

