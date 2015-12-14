{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.PrefixTree
    ()where

import Data.FingerTree
import Data.Monoid

instance Measured (Sum Int, Sum Int) Int where
    measure a = (Sum a, Sum 1)

type PrefixTree = FingerTree (Sum Int, Sum Int) Int

test :: PrefixTree
test = fromList [1,3,4,1,4,-1,9]

prefixAt :: Int -> PrefixTree -> Int
prefixAt index tree =
    case a of
      EmptyR -> 0
      (_ :> b) -> b
    where (left,_ ) = split (\(_,x) -> x -1 > Sum index) tree
          a = viewr $ fmapWithPos (\(Sum x,_) _ -> x) left
