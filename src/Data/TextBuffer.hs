module Data.TextBuffer
    () where

import Prelude hiding (drop, splitAt,length, lines)
import Data.Sequence

type Line = Seq Char
    
type TextBuffer = ( Seq Line, Line, Line, Seq Line)

----------------------------------------------------------------------------
insert (l,il,ir,r) c = (l,il |> c, ir,r)

backspace text@(l,il,ir,r) =
  let (line, col) = getLineCol text
      il'         = drop 1 il
  in case col of
       0 -> mergeWithPrev text
       _ -> (l, il', ir, r)

moveCol (i,il,ir,r) col = (i,il',ir',r)
  where (il',ir')  = splitAt col (il><ir)

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
mergeWithPrev tb@(l, il, ir, r) =
  case viewr l of
    EmptyR -> tb
    (l' :> il') -> (l', il' >< il, ir, r)
                     
----------------------------------------------------------------------------
--helpers
merge (l,il,ir,r) = (l |> il) >< (ir <| r)

