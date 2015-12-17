{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.PrefixTree
    ( PrefixTree
    , splitAtIndex
    , prefixAt
    , insertAt
    , valueAt
    , module Data.FingerTree 
    , size
    ) where

import Data.FingerTree
import Data.Monoid

        
instance Measured (Sum Int, Sum Int) Int where
    measure a = (Sum a, Sum 1)

type PrefixTree = FingerTree (Sum Int, Sum Int) Int

test :: PrefixTree
test = fromList [1,3,4,1,4,-1,9]

splitAtIndex :: Int -> PrefixTree -> (PrefixTree, PrefixTree)
splitAtIndex index  = split (\(_,i) -> i-1 > Sum index) 

-- this could potentiall be improved 
prefixAt :: Int -> PrefixTree -> Int
prefixAt index tree =
    case a of
      EmptyR   -> 0
      (_ :> b) -> b
    where (left,_ ) = splitAtIndex index tree
          a         = viewr $ fmapWithPos (\(Sum x,_) _ -> x) left


valueAt :: Int -> PrefixTree -> Int
valueAt index tree =
    case a of
      EmptyR   -> 0
      (_ :> b) -> b
    where (left,_ ) = splitAtIndex index tree
          a         = viewr left

insertAt :: Int -> Int -> PrefixTree -> PrefixTree
insertAt index insertee pTree = (left |> insertee) >< right
  where (left, right) = splitAtIndex index pTree

size :: PrefixTree -> Int
size pTree =
  case sTree of
    EmptyR      -> 0
    (_ :> size) -> size
  where sTree = viewr $ fmapWithPos (\(Sum x, _) y -> x + y) pTree
