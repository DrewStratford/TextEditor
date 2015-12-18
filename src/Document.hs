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
    , fromString
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

--------------------------------------------------------------------------
{- 
   insertion and deletion etc,
   acts on the text in the document and then must update the
   lineTracker accordingly
-}

-- | inserts a char into the document at the docs
--  current position. Will also handle newlines & deletion etc

insertChar :: Document -> Char -> Document
insertChar doc c
    | c == '\n' = newLine newDoc
    | c == '\DEL' = removeChar doc
    | otherwise = newDoc
  where insertPoint =  asSeqCoOrd (lines doc)
        newDoc = doc
                 { lines = increase $ lines doc
                 , text = insert (text doc) insertPoint c
                 }
       

newLine :: Document -> Document
newLine doc =
    let ls = lineStart $ cursorDown $ newline $ lines doc
    in doc{ lines = ls}

        
removeChar :: Document -> Document
removeChar doc
    | col == 0 && line == 0 = doc
    | otherwise             =
        doc { text = uText
            , lines = backSpace $ lines doc
            }
  where (line, col) = getPos doc
        removePoint = asSeqCoOrd (lines doc)
        uText       = remove (text doc) removePoint

-- helpers functions for insert etc
insert seq i a = (left |> a) >< right
  where (left, right) = splitAt i seq

remove :: Seq a -> Int -> Seq a
remove seq i =
    case view of
      EmptyR   -> right
      (left :> _) -> left >< right
  where (l, right) = splitAt i seq
        view       = viewr l
                        
fromString :: String -> Document
fromString [] = document
fromString cs = go cs document
  where go :: String -> Document -> Document
        go [] doc  = doc
        go (c:cs) doc = go cs (doc `insertChar` c)



-- | outputs the contents of the document onto
--   the console (only temporary)
output = mapM putChar . text

-- | returns (line, col) of document
getPos doc = (line curs, col curs)
  where curs = lines doc
