module Data.TextBuffer
    ( TextBuffer
    , insert
    , backspace
    , newline
    , moveCol
    , moveRight
    , moveLeft
    , moveLine
    , getLineCol
    , fromStrings
    , getLineSection
    ) where

import Prelude hiding (drop, splitAt,length, lines, take)
import Data.Sequence

type Line = Seq Char
    
type TextBuffer = ( Seq Line, Line, Line, Seq Line)


-- | construction
fromStrings :: [String] -> TextBuffer
fromStrings ss = splitAtLine (fromList $ map fromList ss) 0

----------------------------------------------------------------------------
insert (l,il,ir,r) c = (l,il |> c, ir,r)

backspace text@(l,il,ir,r) =
  let (line, col) = getLineCol text
      (il', _)    = splitAt (col - 1) il
  in case col of
       0 -> mergeWithPrev text
       _ -> (l, il', ir, r)

newline (l,il,ir,r) = (l |> il, empty, ir,r)

moveCol (i,il,ir,r) col = (i,il',ir',r)
  where (il',ir')  = splitAt col (il><ir)

moveLeft (i,il,ir,r) = (i,il',ir',r)
  where (il',c) = splitAt (length il - 1) il
        ir'     = c >< ir
        
moveRight (i,il,ir,r) = (i,il',ir',r)
  where (c,ir') = splitAt 1 ir
        il'     = il >< c

moveLine tb@(_,il,_,_) line = moveCol (splitAtLine (merge tb) line) col
  where col = length il
----------------------------------------------------------------------------
splitAtCol :: Line -> Int -> (Line,Line)
splitAtCol line col = splitAt col line

splitAtLine :: Seq Line -> Int -> TextBuffer
splitAtLine lines line =

    case view of
      EmptyL -> (left, empty, empty, empty)
      (ir :< right) -> (left, empty, ir, right)

   where (left,r) = splitAt line lines
         view     = viewl r

getLineCol :: TextBuffer -> (Int,Int)
getLineCol (l,il,_,_) = (length l, length il)

-- | merges the current line with the previous line
--   the current line is now the prev line + old current line
mergeWithPrev :: TextBuffer -> TextBuffer
mergeWithPrev tb@(l, il, ir, r) =
  case viewr l of
    EmptyR -> tb
    (l' :> il') -> (l', il' >< il, ir, r)
                     

----------------------------------------------------------------------------
-- | draws the section of the textbuffer specified
getLineSection :: Int -> Int -> TextBuffer -> Seq Line
getLineSection x height textBuf =
  let text = merge textBuf
      (_,_,ir,r) = splitAtLine text x
      (segment,_,_,_) = splitAtLine (ir <| r) height
  in segment

----------------------------------------------------------------------------
--helpers
merge (l,il,ir,r) = (l |> (il >< ir)) >< r

