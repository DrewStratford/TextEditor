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
    , getSection
    , removeSection
    , insertSection
    ) where

import Prelude hiding (drop, splitAt,length, lines, take)
import Control.Arrow
import Data.Function

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

moveLeft text = moveCol text (col + 1) 
  where (_, col) = getLineCol text
        
moveRight text = moveCol text (col - 1) 
  where (_, col) = getLineCol text

moveLine :: TextBuffer -> Int -> TextBuffer
moveLine tb@(_,il,_,_) line = moveCol (splitAtLine (merge tb) line) col
  where col = length il

moveLineCol tb line col = moveCol (moveLine tb line) col

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

splitAtLineCol :: TextBuffer -> Int -> Int -> (Seq Line,Seq Line)
splitAtLineCol lines line col = (l |> il, ir <| r)
  where (l,il,ir,r) = moveLineCol lines line col
        
        
-- | gives the current position of the cursor
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
getLineSection x height = fst . splitAt (x + height) . snd . splitAt x . merge

getSection :: Int -> Int -> Int -> Int -> TextBuffer -> TextBuffer
              --TODO make this not rely on maxbound
getSection line col line' col' = removeSection x' y' maxBound 0 >>> removeSection 0 0 x y

  where (x, y)     = min (line,col) (line',col')
        (x', y')   = max (line,col) (line',col')


removeSection :: Int -> Int -> Int -> Int -> TextBuffer -> TextBuffer
removeSection line col line' col' text = moveLineCol (l, il, ir, r) line col

  where (x, y)     = min (line,col) (line',col')
        (x', y')   = max (line,col) (line',col')

        -- finds the sections to the left and right of specified section
        (l,il,_,_) = moveLineCol text x y
        (_,_,ir,r) = moveLineCol text x' y'


insertSection :: TextBuffer -> TextBuffer -> TextBuffer
insertSection text@(l, il, ir, r) insertee =
  let (line, col)   = getLineCol text
      (line', col') = endPoint insertee
      text          = ((l |> il) >< merge insertee, ir, empty, r)
  in  moveLineCol text (line + line') (col + col')
    & mergeWithPrev
    & \ text' -> moveLineCol text' (line+1) 0
    & mergeWithPrev
    

----------------------------------------------------------------------------
--helpers
merge (l,il,ir,r) = (l |> (il >< ir)) >< r

-- gets the point at the end of the text buffer
endPoint text@(l,_,_,r) = getLineCol $ moveLine text (length l + length r)
