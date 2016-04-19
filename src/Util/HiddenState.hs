module HiddenState where

type Func a b = a -> b
type HiddenState output = FixF ((->) output) output

data FixF f a = In (f (FixF f a), a) 

instance Show a => Show (FixF f a) where
  show (In (_,a)) = "(In _ " ++ show a ++ ")"

type StateF state = FixF ((->) state) state

selfReturn :: (t1 -> t -> (t1, a)) -> t1 -> t -> FixF ((->) t) a
selfReturn f state a = In (selfReturn f updatedSt, out)
   where (updatedSt, out) = f state a

get (In a) = a

update a b = (a + 1, show a ++ show b)

test = selfReturn update 0 1

makeHiddenState :: (hState -> hState) -> (hState -> out -> out) -> hState -> out -> HiddenState out
makeHiddenState hiddenUpdt outputUpdt = selfReturn go
  where go s a = (hiddenUpdt s, outputUpdt s a)

                       {-
hide :: FixF ((->) t) a -> _
hide (In (s,_)) =  s
-}
