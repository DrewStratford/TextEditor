module Util.HiddenState
       ( HiddenState
       , update
       , makeHiddenState
       , getState
       ) where

{-
Essentially forms a closure that "updates" it's variable for each call of the function
using update. This may be used to have objects with different types of state
in one collection as the closure will hide the type of the variable.

TODO: consider if this is worthwile compared to ExistentialQuantification
UPDATE: Pretty sure this isn't as good as existentials as it is a lot harder
to get output's of a different type. This probably could be fixed using a functor
instance similar to state but I don't think that it is worth it.
-}

type HiddenState input output = FixF ((->) (input,output)) output

data FixF f a = In (f (FixF f a), a) 

instance Show a => Show (FixF f a) where
  show (In (_,a)) = "(In _ " ++ show a ++ ")"

selfReturn ::
  (hState -> (input, output) -> (hState, output)) ->
  hState ->
  (input, output) ->
  HiddenState input output
selfReturn f state input  = In (selfReturn f updatedSt, out)
   where (updatedSt, out) = f state input

{- | returns the state of a hidden state -}
getState :: FixF ((->) a) b -> b
getState (In (_, state)) = state

{- | updates the state of HiddenState for a given input -}

update :: HiddenState input output -> output -> input -> HiddenState input output
update (In (f, _)) output input = f (input, output)

{- | Constructs a Hidden with a function to update the hidden state,
     and function to update the output, which are both based on the
     passed input, best to pass functions and initial hidden state and
     then call when appropriate
-}
makeHiddenState :: (hState -> input -> hState)        -- ^ updates hiddenState 
                -> (hState -> input -> out -> out)    -- ^ updates output
                -> hState                             -- ^ initial hiddenstate
                -> input
                -> out
                -> HiddenState input out
makeHiddenState hiddenUpdt outputUpdt hState input out = selfReturn go hState (input, out)
  where go state (input, output) = (hiddenUpdt state input, outputUpdt state input output)

