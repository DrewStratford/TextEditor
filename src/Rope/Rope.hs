{-# LANGuAGE GADTs, OverloadedStrings #-}

module Rope where

import qualified Data.Text as T

-- The size of the leaf blocks
wordBlock = 8

data Rope = Branch Int Rope Rope | Leaf Int T.Text deriving (Show)

------------------------------------------------------------------------------------
--operations on Characters
insertChar :: Char -> Int -> Rope -> Rope
insertChar c i (Branch len left right)
         | i < len = Branch (len + 1) (insertChar c i left) right
         | otherwise = Branch len left (insertChar c (i - len) right)
insertChar c i (Leaf len text)
         | i <= len  = splitLeaf $ Leaf (len + 1) $ placeAt c i text  
         | otherwise = error "index doesn't exist"

deleteChar :: Int -> Rope -> Rope
deleteChar i (Leaf len text) = Leaf (len - 1) $ removeAt i text
deleteChar i (Branch len left right)
         | i < len = Branch (len - 1) (deleteChar i left) right
         | otherwise = Branch len  left (deleteChar i right)

-- splitLeafs a Leaf node if required
splitLeaf :: Rope -> Rope
splitLeaf r@Branch{}  = r
splitLeaf leaf@(Leaf len text)
      | len < wordBlock = leaf
      | otherwise = Branch wordBlock left right
                    where left  = Leaf len l
                          right = Leaf 1   r
                          (l,r) = T.splitAt wordBlock text
                                  
------------------------------------------------------------------------------------
-- creation stuff
empty = Leaf 0 ""
------------------------------------------------------------------------------------
size (Branch s _ _) = s
size (Leaf s _)     = s

removeAt :: Int -> T.Text -> T.Text
removeAt i text =
  let (l,r) = T.splitAt i text
  in T.append l $ T.tail r
     
-- places a char in the middle of text
placeAt :: Char -> Int -> T.Text -> T.Text
placeAt c i text =
  let (l,r) = T.splitAt i text
  in T.append l $ T.append(T.pack [c]) r

------------------------------------------------------------------------------------
