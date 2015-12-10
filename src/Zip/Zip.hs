module Zip
    ( left
    , right
    , atPoint
    , empty
    , insert
    ) where

type Zip a = ([a], [a])

left (a:as, bs) = (as, a:bs)
left z = z

right (as, b:bs) = (b:as, bs)
right z = z

atPoint :: (a -> a) -> Zip a -> Zip a
atPoint _ z@([],_) = z
atPoint f (a:as,bs) = (f a: as,bs)

empty = ([],[])

insert a (as,bs) = (a:as,bs)
