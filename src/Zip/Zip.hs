module Zip.Zip
    ( left
    , right
    , Zip 
    , toPoint
    , withPoint
    , zipper
    , insert
    , replace
    , getPoint
    , delete
    ) where

import Data.Default

type Zip a = ([a], a, [a])

left (a:as, b, bs) = (as, a, b:bs)
left z = z

right (as, a, b:bs) = (a:as, b, bs)
right z = z

toPoint :: (a -> a) -> Zip a -> Zip a
toPoint f (as, m, bs) = (as, f m, bs)

withPoint :: (a -> b) ->  Zip a -> b
withPoint f (_, a, _) = f a

zipper a = ([], a, [])

insert a (ls, c, rs) = (c:ls, a, rs)

replace :: a -> Zip a -> Zip a
replace a = toPoint (const a)

getPoint :: Zip a -> a
getPoint (_, a, _) = a


{- | Removes the current point and moves the left. If there are
     no as to the left then it takes the next to the right
     if there are no remaining items places default in
     the pointer
-}
delete :: Zip a -> Zip a
delete ([],_,[]) = error "implement a proper defualt for TextState then fix this.\
                          \ You should probably move VTY out of TextState"
delete (x:xs, _, rs) = (xs, x, rs)
delete ([], _, r:rs) = ([], r, rs)
