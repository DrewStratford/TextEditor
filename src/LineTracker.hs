module LineTracker
    ( LineTracker
    , lineTracker
    , up
    , down
    , new
    ) where

import Zip.Zip

{-
  line is where the newline is in the string
  however when a newline is inserted above another
  newline line is offset, offset is the offset that the line
  causes below
-}
data NewLine = NewLine { place :: Int, offset :: Int, offsetWhenCreated :: Int }

data LineTracker = LineTracker (Zip NewLine) Int Int

lineTracker = LineTracker (NewLine 0 0 0 `insert` zipper)

up (LineTracker z off line) = LineTracker (left z) o lineNum
  where o = off - withPoint offset 0 z
        lineNum
            | atStart z = 0
            | otherwise = line - 1

down (LineTracker z off line) = LineTracker (left z) o lineNum
  where o = off + withPoint offset 0 z - withPoint offsetWhenCreated 0 z 
        lineNum
            | atEnd z = line
            | otherwise = line + 1

