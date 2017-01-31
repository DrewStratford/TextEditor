{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, BangPatterns #-}
module Data.Buffer
  ( Buffer
  , measure
  , size
  , append
  , fromText
  , fromString
  , Data.Buffer.splitAt
  , splitAtLine
  , toLines
  , take
  , drop
  , takeEnd
  , dropEnd
  , insertAt
  , deleteAmountAt
  , toText
  , atEOL
  , atEnd
  ) where


{-
  A new Buffer that uses contigous text segments.
  Should be easier on the cache
-}


import Prelude hiding (take, drop, lines)
import Control.Arrow
import Data.FingerTree
import Data.Monoid
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

-- | The max chunksize for each Text segment (should probably be much higher)
chunkSize = 256
nl = fromText "\n"

countNLs = T.count "\n"

fromText = fromList . T.chunksOf chunkSize
fromString = fromText . T.pack

toList :: Buffer -> [T.Text]
toList = foldr (:) []

toText = T.concat . toList

size :: Buffer -> Int
size = columns . measure


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
-- doesn't work in some subtle case
-- works whenever each line is < chunksize. This is because the 1st case of the
-- if branch we only look back one chunk whereas we should look back until the first
-- newline throughout many possible chunks
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

-- helpers
both f (a,b) = (f a, f b)

splitNLines :: Int -> T.Text -> (T.Text, T.Text)
splitNLines n text = T.concat `both` Prelude.splitAt n asLines
  where asLines = addNLs $ T.splitOn "\n" text

        --adds a nl to all but the last element
        addNLs [] = []
        addNLs [x] = [x]
        addNLs (x:xs) = (x `T.snoc` '\n') : addNLs xs
