{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.TextBuffer
    ( TextBuffer
    , splitAtIndex
    , module Data.FingerTree 
    ) where

import Prelude hiding (getLine)
    
import Data.FingerTree
import Data.Monoid

{-
  in the measure for this tree the right hand side is the amount
  of lines in the text while the left is the index
-}

instance Measured (Sum Int, Sum Int) Char where
    -- to consider: should newlines count towards index?
    measure '\n' = (Sum 0, Sum 1)
    measure  _   = (Sum 1, Sum 0)

type Text = FingerTree (Sum Int, Sum Int) Char
type TextBuffer = (Text, Text)

--------------------------------------------------------
test :: Text
test = fromList "abcde\nfgh\nijk"

test2 = fromList "\nabcde"
        
--------------------------------------------------------
--helpers
splitAtLineCol :: Int -> Int -> Text -> TextBuffer
splitAtLineCol line col text = (left >< ll, right >< lr)
  where (left, l, right) = getLine line text
        (ll,       lr  ) = splitAtIndex col l
                   
-- | splits at the left of the index given
splitAtIndex :: Int -> Text -> TextBuffer
splitAtIndex index  = split (\(i,_) -> i > Sum index) 

splitAtLine :: Int -> Text -> TextBuffer
splitAtLine index = split (\(_,i) -> i >= Sum index)

-- | gives the (preceeding, line, proceeding) requested
--  where line is specified by the variable index
getLine :: Int -> Text -> (Text,Text,Text)
getLine index text = (left, line, right)
  where (left, r) = splitAtLine index text
        (line, right) = splitAtLine (index+1) r

--------------------------------------------------------

insert :: TextBuffer -> Char -> TextBuffer
insert (l,r) c = (l |> c, r)

remove :: TextBuffer -> TextBuffer
remove tb@(l,r) =
  case view of
    EmptyR -> tb
    (left :> _) -> (left, r)
  where view = viewr l
