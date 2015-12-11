module LineTracker
    ( LineTracker
    , lineTracker
    , up
    , down
    , newline
    , asSeqCoOrd
    , getLineNum
    ) where

import Prelude hiding (lines)
import Zip.Zip

{-
  line is where the newline is in the string
  however when a newline is inserted above another
  newline line is offset, offset is the offset that the line
  causes below
-}
data NewLine = NewLine
    { place :: Int,
      offset :: Int,
      offsetWhenCreated :: Int
    } deriving Show

data LineTracker = LineTracker
    {
      lines         :: Zip NewLine,
      offsetAccrued :: Int,
      point         :: Int
    } deriving Show 

lineTracker = LineTracker (NewLine 0 0 0 `insert` zipper) 0 0

up (LineTracker z off line) = LineTracker (left z) o lineNum
  where o = off - getOffset z
        lineNum
            | atStart z = 0
            | otherwise = line - 1

down (LineTracker z off line) = LineTracker (right z) o lineNum
  where o = off + getOffset z 
        lineNum
            | atEnd z = line
            | otherwise = line + 1

newline :: LineTracker -> Int -> LineTracker
newline lTracker col = down lTracker { lines = updtdLines } -- should probably call down here
  where prevNewLine = getPlace $ lines lTracker 
        lineNum     = col + prevNewLine
        nLine       = NewLine lineNum col $ offsetAccrued lTracker
        updtdLines  = left $ nLine `insert` lines lTracker
        
asSeqCoOrd :: LineTracker -> Int
asSeqCoOrd lTracker = offsetAccrued lTracker + nLine - getOffsetWhenCreated (lines lTracker)
  where nLine = getPlace $ lines lTracker

getLineNum = point 
-- helper for working with lines
getPlace :: Zip NewLine -> Int
getPlace = withPoint place 0


getOffset = withPoint offset 0
getOffsetWhenCreated = withPoint offsetWhenCreated 0
