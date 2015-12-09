{-# LANGuAGE GADTs, OverloadedStrings #-}

module Rope
    ( Rope
    , insert
    , fromString
    , empty
    , delete
    , deleteChar
    , split
    ) where 

import qualified Data.Text as T

-- The size of the leaf blocks
wordBlock :: Int
wordBlock = 8

empty = Null
        
data Rope = Branch Int Rope Rope
          | Null
          | Leaf Int T.Text deriving (Show)

------------------------------------------------------------------------------------
-- operations on strings

combine Null right = right --doesn't combine if one node is empty
combine left Null  = left
combine left@(Leaf len1 text1) right@(Leaf len2 text2)
      | len1 + len2 <= wordBlock = Leaf (len1 + len2) $ T.append text1 text2
      | otherwise = Branch len1 left right
-- standard combine of two branches
combine left right = Branch len left right
  where len = size left
                    

split :: Int -> Rope -> (Rope, Rope)
split _ Null = (Null, Null)
split 0 rope = (Null,rope)
split i (Branch len left right)
      -- right branch must be in part though members in left can be in right
      | i <= len = (ll, lr `combine` right) 
      -- split can only be in right though members in right may be in left part
      | otherwise = (left `combine` rl, rr) 
  where (ll, lr) = split i left
        (rl, rr) = split (i - len) right
                   
split i leaf@(Leaf len text)
      | i == 0 = (Null, leaf)
      | i >= wordBlock || i >= len = (leaf, Null)
      | otherwise = (Leaf lLen left, Leaf rLen right)
  where rLen = len - i
        lLen = i
        (left, right) = T.splitAt i text

insert :: Rope -> Int -> Rope -> Rope
insert inserting index rope = left `combine` inserting `combine` right
  where (left, right) = split index rope

insertChar :: Char -> Int -> Rope -> Rope
insertChar c = insert leaf
  where leaf = Leaf 1 (T.pack [c])

delete :: Int -> Int -> Rope -> Rope
delete start range rope = left `combine` right
  where (left, mid) = split start rope
        (_, right)  = split range mid

deleteChar :: Int -> Rope -> Rope
deleteChar index = delete index 0
                   
------------------------------------------------------------------------------------
-- creation stuff

gen :: String -> [Int] -> Rope
gen cs is = foldl (\r (c, i) -> insertChar c i r) Null (zip cs is)

-- this will probobably be slow
-- possibly speed it up by inserting wordBlock 
fromString :: String -> Rope
fromString input = frmString input 0 Null
  where frmString [] _ rope = rope
        frmString cs i r = frmString rcs (i+wordBlock) (insert word i r)
          where (w, rcs) = splitAt wordBlock cs
                word = Leaf (length w) $ T.pack w
                   
                               
------------------------------------------------------------------------------------
-- helper functions
size :: Rope -> Int
size (Branch s _ r) = s + size r
size (Leaf s _)     = s
size Null = 0

------------------------------------------------------------------------------------
-- balancing stuff

{-
    a naive balance
    splits tree in half then combines it
    
    has a space issue in that an extra node is made
    plus bad time complexity
-}
balance :: Rope -> Rope
balance Null = Null
balance l@Leaf{} = l
balance branch =
  let i = size branch `div` 2
      (l, r) = split i branch
  in Branch i (balance l) (balance r)
      
