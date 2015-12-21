{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.TextBuffer
    ( TextBuffer
    , module Data.FingerTree 
    , insert
    , remove
    , jumpTo
    , output
    , fromString
    , toString
    , getLineCol
    ) where

import Prelude hiding (getLine)
    
import Data.Foldable
    
import Data.FingerTree
import Data.Monoid

-- sum of previous lines, line, Index
instance Monoid (Int,Int,Int) where
    mempty = (0,0,0)
    (a,b,c) mappened (x,y,z)
            | a < x = undefined
            | otherwise (a+x,b+y,c+z)
{-
  in the measure for this tree the right hand side is the amount
  of lines in the text while the left is the index
-}

instance Measured (Sum Int, Sum Int) Char where
    -- to consider: should newlines count towards index?
    measure '\n' = (Sum 0, Sum 1)
    measure  _   = (Sum 1, Sum 0)

-- this is so we can extract coordinates
-- using fmap & view
instance Measured () (Int, Int) where
    measure _ = ()
                
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

-- | jumps to the line & col in the buffer
--  if not a valid position it will jump to the closest
--  line & col below the argument
jumpTo :: Int -> Int -> TextBuffer -> TextBuffer
jumpTo line col (l,r) = splitAtLineCol line col (l >< r)


--------------------------------------------------------
-- output etc
output :: TextBuffer -> IO ()
output (l,r) = mapM_ putChar (l >< r)

-- | finds the coordinates at the cursor,
--   should be O(1) due to fmap being lazy but I'm not sure.
getLineCol :: TextBuffer -> (Int, Int)
getLineCol (text, _) =
  case view of
    EmptyR     -> (0,0)
    (_ :> pos) -> pos
  where view = viewr $ fmapWithPos (\(Sum l, Sum c) _ -> (l,c)) text

fromString cs = (empty, fromList cs)

toString (l,r) = toList (l >< r)

