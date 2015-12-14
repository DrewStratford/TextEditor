{-# LANGUAGE OverloadedStrings #-}
module Document
    ( Document
    , document
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , insertChar
    , output
    , getPos
    )where

import Prelude hiding (lines, splitAt)
import Data.Sequence

import LineTracker

data Document = Doc
    { lines :: LineTracker,
      text  :: Seq Char
    } deriving Show

document = Doc lineTracker empty
           
moveUp doc = doc{ lines = cursorUp $ lines doc}
moveDown doc = doc{ lines = cursorDown $ lines doc}

moveLeft doc = doc{ lines = cursorLeft $ lines doc}
moveRight doc = doc{ lines = cursorRight $ lines doc}

newLine :: Document -> Document
newLine doc = doc
              { lines = newline $ lines doc}

insertChar :: Document -> Char -> Document
insertChar doc c
    | c == '\n' = newLine newDoc
    | otherwise = newDoc
  where insertPoint =  asSeqCoOrd (lines doc)
        newDoc = doc
                 { lines = increase $ lines doc
                 , text = insert (text doc) insertPoint c
                 }
        
insert seq i a = (left |> a) >< right
  where (left, right) = splitAt i seq


output = (mapM putChar) . text

getPos doc = (line curs, col curs)
  where curs = lines doc
