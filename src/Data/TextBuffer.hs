module Data.TextBuffer
    ( TextBuffer
    , insert
    , backspace
    , delete
    , newline
    , moveLineCol
    , moveCol
    --, moveRight
    --, moveLeft
    --, moveUp
    --, moveDown
    --, moveLine
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
insert :: TextBuffer -> Char -> TextBuffer
insert (l,il,ir,r) c = (l,il |> c, ir,r)

delete :: TextBuffer -> TextBuffer
delete text@(l,il,ir,r)
    | col == lineSize text = mergeWithNext text
    | otherwise            = (l,il, drop 1 ir, r)
  where (_, col) = getLineCol text

backspace :: TextBuffer -> TextBuffer
backspace text@(l,il,ir,r)
    | col == 0 = mergeWithPrev text
    | otherwise = (l, take (col - 1) il, ir, r)
  where (_, col) = getLineCol text


newline (l,il,ir,r) = (l |> il, empty, ir,r)

moveCol (i,il,ir,r) col = (i,il',ir',r)
  where (il',ir')  = splitAt col (il><ir)

moveLine :: TextBuffer -> Int -> TextBuffer
moveLine tb@(_,il,_,_) line = moveCol (splitAtLine (merge tb) lineActual) col
  where col = length il
        -- we stop the cursor from going out of bounds
        lineActual = min (lineAmount tb - 1) line

moveLineCol :: TextBuffer -> Int -> Int -> TextBuffer
moveLineCol tb line col = moveCol (moveLine tb line) col

----------------------------------------------------------------------------

splitAtLine :: Seq Line -> Int -> TextBuffer
splitAtLine lines line =

    case view of
      EmptyL -> (left, empty, empty, empty)
      (ir :< right) -> (left, empty, ir, right)

   where (left,r) = splitAt line lines
         view     = viewl r

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
                     

mergeWithNext :: TextBuffer -> TextBuffer
mergeWithNext tb@(l, il, ir, r) =
  case viewl r of
    EmptyL -> tb
    (ir' :< r') -> (l, il, ir >< ir', r')

----------------------------------------------------------------------------
-- | gets the section of the textbuffer specified
getLineSection :: Int -> Int -> TextBuffer -> Seq Line
getLineSection x height = snd . splitAt x  . fst . splitAt (x + height) . merge

getSection :: (Int, Int) -> (Int, Int) -> TextBuffer -> TextBuffer
getSection point1 point2  text = removeSection (0, 0) startPoint endRemoved
  where startPoint = min point1 point2
        midPoint   = max point1 point2
        endPoint   = endLineCol text 
        endRemoved = removeSection midPoint endPoint text


removeSection :: (Int, Int) -> (Int, Int) -> TextBuffer -> TextBuffer
removeSection (line, col) (line', col') text = moveLineCol (l, il, ir, r) line col

  where (startLine, startCol) = min (line,col) (line',col')
        (endLine, endCol)     = max (line,col) (line',col')

        -- finds the sections to the left and right of specified section
        (l,il,_,_) = moveLineCol text startLine startCol
        (_,_,ir,r) = moveLineCol text endLine endCol


insertSection :: TextBuffer -> TextBuffer -> TextBuffer
insertSection text@(l, il, ir, r) insertee =
  let (line, col)   = getLineCol text
      (line', col') = endLineCol insertee
      il'           = adjust (il ><) 0 $ merge insertee
      text'         = (l >< il', empty, ir, r)
  in  moveLineCol text' (1+line + line') 0
     & mergeWithPrev
      -- sets cursor to the end of the pasted section
     & \tb -> moveLineCol tb (line + line') (col + col')
      
    

----------------------------------------------------------------------------
-- searching functions

----------------------------------------------------------------------------
--helpers
merge (l,il,ir,r) = (l |> (il >< ir)) >< r

lineAmount (l,_,_,r) = length l + length r + 1

lineSize (_, il, ir, _) = length il + length ir

-- gets the point at the end of the text buffer
endLineCol :: TextBuffer -> (Int, Int)
endLineCol = getLineCol . moveToEndOfLine . moveToEnd

moveToEnd :: TextBuffer -> TextBuffer
moveToEnd text@(l,_,_,r) = moveLine text (length l + length r)

moveToEndOfLine :: TextBuffer -> TextBuffer
moveToEndOfLine text@(_,il,ir,_) = moveCol text (length il + length ir)
