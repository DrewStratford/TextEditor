{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Buffer
  ( Buffer
  , measure
  , size
  , lineCount
  , append
  , fromText
  , toText
  , fromString
  , Data.Buffer.splitAt
  , splitAtLine
  , toLines
  , take
  , takeLines
  , drop
  , dropLines
  , takeEnd
  , dropEnd

  , insertAt
  , deleteAmountAt
  , pointToCursor
  , cursorToPoint

  , atEOL
  , atEnd

  , expandTabs
  ) where


{-
  A new Buffer that uses contigous text segments.
  Should be easier on the cache
-}


import Prelude hiding (take, drop, lines)
import qualified Prelude as P
import Control.Arrow
import Data.FingerTree
import Data.Monoid
import Data.Maybe
import qualified Data.Text as T

data Size = Size { chunks  :: !Int
                 , columns :: !Int
                 , lines   :: !Int
                 } deriving (Show)

instance Monoid Size where
  mempty = Size 0 0 0
  (Size cs c l) `mappend` (Size cs' c' l') = Size (cs + cs') (c + c') (l + l')

instance Measured Size T.Text where
  measure text = Size 1 (T.length text) (countNLs text)


type Buffer = FingerTree Size T.Text

-- | The max chunksize for each Text segment 
chunkSize = 1048
nl = fromText "\n"

countNLs = T.count "\n"

fromText = fromList . T.chunksOf chunkSize
fromString = fromText . T.pack

toList :: Buffer -> [T.Text]
toList = foldr (:) []

toText = T.concat . toList

size :: Buffer -> Int
size = columns . measure

lineCount :: Buffer -> Int
lineCount = lines . measure

append :: Buffer -> Buffer -> Buffer
append lBuff rBuff = case (viewr lBuff, viewl rBuff) of
  (EmptyR, _) -> rBuff
  (_, EmptyL) -> lBuff
  (l :> lm, rm :< r) ->
    let combinedSize = T.length lm + T.length rm
    in if combinedSize <= chunkSize
          then (l |> (lm `T.append` rm)) >< r
          else lBuff >< rBuff


splitAt :: Int -> Buffer -> (Buffer, Buffer)
splitAt 0 buffer = (mempty, buffer)
splitAt c buffer =
  let (l,r) = split ((>c) . columns) buffer
      imbalance = c - columns (measure l)
  in case viewl r of
    EmptyL   -> (l, mempty)
    ir :< or -> let (left, right) = fromText `both` T.splitAt imbalance ir
                in (l `append` left, right `append` or)


toLines buffer =
  let (l, r) = splitAtLine 1 buffer
  in if r == mempty
  then [l]
  else l : toLines r

  
splitAtLine c buffer
  | c <= 0 = (mempty, buffer)
  | otherwise = splitAtLine' (c) buffer

-- TODO: Works (I think), generate test and add better documentation.
splitAtLine' :: Int -> Buffer -> (Buffer, Buffer)
splitAtLine' c buffer =
  let (l,r) = split ((>c) . lines) buffer
      imbalance = c - linesInL
      linesInL  = lines (measure l)
  in if imbalance <= 0

  -- In this case we need to remove any over hanging chars (e.g. "one\n[two]") from
  -- the left section
  then let (extraL, extraR) =  breakOnEnd "\n" l
       in (extraL, extraR `append` r)

  -- In this case we need to split the right section as this contains at least one
  -- line that should be in the left

  -- TO CONSIDER: use append instead of |> as this may reduce fragmentation; though, it will
  -- incur extra cost.
  else case viewl r of
    EmptyL   -> (l, mempty)
    ir :< or -> let (left, right) = splitNLines imbalance ir
                in  (l |> left, right <| or)


insertAt :: Int -> Buffer -> Buffer -> Buffer
insertAt i insertee buffer =
  let (l, r) = Data.Buffer.splitAt i buffer
  in l `append` insertee `append` r
  
deleteAmountAt :: Int -> Int -> Buffer -> Buffer
deleteAmountAt  amount i buffer =
  let (l, r) = Data.Buffer.splitAt i buffer
      s      = size l
  in (Data.Buffer.take (s - amount) l) `append` r


breakOnEnd :: T.Text -> Buffer -> (Buffer, Buffer)
breakOnEnd pattern buffer = breakOnEnd' pattern (buffer, mempty)

breakOnEnd' pattern (buff, acc) = case viewr buff of
    EmptyR -> (buff, acc)
    or :> ir -> let (remainder, taken) = fromText `both` T.breakOnEnd pattern ir
                in if mempty == remainder
                   then breakOnEnd' pattern (or, taken `append` acc)
                   else (or `append` remainder, taken `append` acc)


-- | Returns the distance till the next instance of pattern.
--   If the pattern doesn't exist returns 0
findIndex :: Char -> Buffer -> Maybe Int
findIndex pattern buffer = findIndex' ((==) pattern) buffer 0

findIndex' pattern buff c = case viewl buff of
    EmptyL   -> Nothing
    t :< ts  -> let index = T.findIndex pattern t
                    leng  = T.length t
                in case index of
                     Just a -> Just (a + c)
                     Nothing -> findIndex' pattern ts (c + leng)


-- | Same as findIndex but from the left.
--   I can't think of many uses for this apart from making certain things faster (i.e. finding cursor).
findIndexEnd :: Char -> Buffer -> Maybe Int
findIndexEnd pattern buffer = findIndexEnd' ((==) pattern) buffer 0

findIndexEnd' pattern buff c = case viewr buff of
    EmptyR   -> Nothing
  -- Ideally I'd like to do this without the reverse as I don't think it is subject to fusion
    ts :> t  -> let index = T.findIndex pattern (T.reverse t)
                    leng  = T.length t
                in case index of
                     Just a -> Just (a + c)
                     Nothing -> findIndexEnd' pattern ts (c + leng)

count :: T.Text -> Buffer -> Int
count pattern = foldl (+) 0 . fmap (T.count pattern) . toList


-- | Transforms the point to (rows, cols)
-- TO CONSIDER: a version that doesn't account for tabs
pointToCursor :: Int -> Int -> Buffer -> (Int, Int)
pointToCursor point indent buffer =
  let (l, _) = Data.Buffer.splitAt point buffer
      line = snd $ breakOnEnd "\n" l
      lineCount = lines $ measure l
      col    = length $ expandTabs indent $ T.unpack $ toText line
  in (lineCount, col)


-- TODO: this doesn't work when moving to a line with a tab
-- TODO: pass in tab
cursorToPoint :: Int -> (Int, Int) -> Buffer -> Int
-- cursorToPoint _ (0, c) buffer = c
cursorToPoint indent (l, c) buffer =
  let ls = takeLines (l + 1) buffer
      (left, right) = splitAtLine l ls
      cols = min c (size right - 1)
  --in size left + cols
  -- needs tab width to work
  in size left + findColWithTabs indent c right
      

take n = fst . Data.Buffer.splitAt n
drop n = snd . Data.Buffer.splitAt n 

takeEnd n buffer = snd $ Data.Buffer.splitAt n' buffer
  where n' = size buffer - n

dropEnd n buffer = fst $ Data.Buffer.splitAt n' buffer
  where n' = size buffer - n


takeLines n = fst . splitAtLine n
dropLines n = snd . splitAtLine n

substr start len = take len . drop start

atEOL c = (== nl) . substr c 1 

atStart c = const (c == 0)
atEnd :: Int -> Buffer -> Bool
atEnd c = (== c) . columns . measure 

c = fromString "\n1234"
d = a `append` b `append` c
a = fromString "one\ntwo\nthree"
b = fromString "\nFOUR\nFIVE"

test = fromString "A job interview for a janitorial position\n-----------------------------------------"
tabTest = fromString "a\tbased"

-- helpers
both f (a,b) = (f a, f b)

splitNLines :: Int -> T.Text -> (T.Text, T.Text)
splitNLines n text = T.concat `both` Prelude.splitAt n asLines
  where asLines = addNLs $ T.splitOn "\n" text

        --adds a nl to all but the last element
        addNLs [] = []
        addNLs [x] = [x]
        addNLs (x:xs) = (x `T.snoc` '\n') : addNLs xs


{- | Expands out all the strings in the string by the given indent amount.
     TODO: consider giving a list of tabstops of varying amount
-}
expandTabs :: Int -> String -> String
expandTabs indent = expandTabs' indent 0


expandTabs' _ _ [] = []
expandTabs' indent i ('\t' : cs) =
  let m = (nextTabStop indent i) - i
      padding = P.take m $ repeat ' '
  in padding ++ expandTabs' indent (i + m) cs

expandTabs' indent i ('\n' : cs) = '\n' : expandTabs' indent 0 cs
expandTabs' indent i (c : cs) = c : expandTabs' indent (i+1) cs


nextTabStop :: Int -> Int -> Int
nextTabStop tabSize i = (1 + (i `div` tabSize)) * tabSize

-- folds on the underlying text (using Data.Text's fold on the chunks)
foldRBuffer :: (Char -> a -> a) -> a -> Buffer -> a
foldRBuffer f def (viewl -> t :< ts) =
  let tail = foldRBuffer f def ts
  in T.foldr f tail t
foldRBuffer _ def _ = def  


foldLBuffer :: (a -> Char -> a) -> a -> Buffer -> a
foldLBuffer f def (viewl -> t :< ts) =
  let tail = foldLBuffer f def ts
  in T.foldl f tail t
foldLBuffer _ def _ = def  


-- this is to find the 'limit' column when dealing with tabs
-- not sure what to call this as it is pretty insane, I think it works though.
-- We could make a much simpler version using toString and basic recursion, but that is not as cool
-- (or as fast?).
findColWithTabs :: Int -> Int -> Buffer ->  Int
findColWithTabs indent limit buff = snd $ foldRBuffer g id buff (0,0)
  -- we use a pretty complicated fold to generate a function that will keep track of the visual and
  -- actual column. Note we "terminate" the function by not evaluating the cont when we come across
  -- a \n, this is to ensure that we don't skip lines.
  where g '\n' _ (i, col) = (i, col)
        g c cont (i, col) = if f c i > limit then (i, col) else cont (f c i, col + 1)
        f '\t' i = nextTabStop indent i
        f _    i =  i +1
    
