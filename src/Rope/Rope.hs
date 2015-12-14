{-# LANGuAGE GADTs, OverloadedStrings #-}

module Rope
    ( Rope
    , insert
    , insertChar
    , empty
    , delete
    , deleteChar
    , split
    ) where 



empty = Null
        -- Left weight, right weight, left branch, right branch
data Rope b = Branch Int b (Rope b) (Rope b)
          | Null
          | Leaf Int Int deriving (Show)

data Node = Node {left :: Int, right :: Int}

type Prefix = Rope Node
                  
------------------------------------------------------------------------------------
{-
-}
combine Null b = b
combine b Null = b
combine l r    = Branch 0 node l r
  where node = Node (size l) (size r)
                    

split :: Int -> Prefix -> (Prefix, Prefix)
split 0 b = (Null, b)
split _ Null = (Null, Null)
split _ l@Leaf{} = (l, Null)
split i branch@(Branch _ node l r)
      | i == size branch = (Null, branch)
      | i < left node    = (ll, lr `combine` r)
  where (ll,lr) = split i l
        (rl,rr) = split (i - left node) r

insert :: Prefix -> Int -> Prefix -> Prefix
insert = undefined


insertChar :: Char -> Int -> Prefix -> Prefix
insertChar = undefined

delete :: Int -> Int -> Prefix -> Prefix
delete = undefined

deleteChar :: Int -> Prefix -> Prefix
deleteChar index = delete index 0
                   
------------------------------------------------------------------------------------
-- helper functions
size :: Prefix -> Int
size Null   = 0
size Leaf{} = 1
size (Branch _ n _ _) = left n + right n
                               
getRight :: Prefix -> Int
getRight Null = 0
getRight Leaf{} = 0
getRight (Branch _ n _ _) = right n

getLeft :: Prefix -> Int
getLeft Null = 0
getLeft Leaf{} = 0
getLeft (Branch _ n _ _) = left n
------------------------------------------------------------------------------------

------------------------------------------------------------------------------------
