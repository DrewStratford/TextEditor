module Zip.Zip
    ( left
    , right
    , Zip 
    , toPoint
    , withPoint
    , zipper
    , insert
    , atStart
    , atEnd
    ) where

type Zip a = ([a], [a])

left (a:as, bs) = (as, a:bs)
left z = z

right (as, b:bs) = (b:as, bs)
right z = z

toPoint :: (a -> a) -> Zip a -> Zip a
toPoint _ z@([],_) = z
toPoint f (a:as,bs) = (f a: as,bs)

withPoint :: (a -> b) -> b ->  Zip a -> b
withPoint _  a ([],_) = a
withPoint f _ (a:_,_) = f a


zipper = ([],[])

insert a (as,bs) = (a:as,bs)

atStart ([],_) = True
atStart _      = False

atEnd (_,[])   = True
atEnd _        = False
