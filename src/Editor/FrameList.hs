module Editor.FrameList
       ( moveDown
       , moveUp
       , moveHorizontal
       , moveVertical
       , split
       , getWindow
       , setWindow
       , getAtWindow
       , Dir (..)
       , SplitType (..)
       , unzip
       , toZip
       , Frame
       ) where

{-
   The frameList is essentially a binary tree of buffers (or pointers to buffers
   we use a zipper to navigate the buffers with the hole being at the buffer currently being edited
-}

import Prelude hiding (Left, Right, unzip)
--import Data.TextBuffer

type Frame = WindowZip String
------------------------------------------------------------------------------------------------------
-- holds the entire editor state


------------------------------------------------------------------------------------------------------
data SplitType = Vert | Hori deriving (Show, Eq)

data Window a
    = Leaf    a
    | Split SplitType (Window a) (Window a) deriving (Show, Eq)

------------------------------------------------------------------------------------------------------
-- | zipper over the window

data Dir
    = UP 
    | DOWN deriving (Show, Eq)

data WindowZip a
    = Zip  (WindowZip a) (Window a) Dir
    | Root (Window a) deriving (Show, Eq)

toZip a = Root $ Leaf a

unzip wZip =
  let test x =
        case x of
          Root{} -> True
          _      -> False
  in getWindow $ (moveUp `till` test) wZip
---------------------------------------------------------------------------------------

moveUp :: WindowZip a -> WindowZip a
moveUp r@Root{}       = r
moveUp (Zip wz w dir) =
  let merged = join (getWindow wz) w dir
  in case wz of
       (Zip wz _ dir) -> Zip wz merged dir
       _              -> Root merged


moveDown :: WindowZip a -> Dir -> WindowZip a
moveDown wZip dir =
  let newLayer w = Zip wZip w dir
  in case getWindow wZip of
       Leaf{}        -> wZip
       (Split _ u d) -> if dir == UP then newLayer u else newLayer d

sink dir = (\x -> moveDown x dir) `till` isLeaf
  where isLeaf wZip =
          case getWindow wZip of
            Leaf{} -> True
            _      -> False

---------------------------------------------------------------------------------------
-- moving between neighbouring windows
moveVertical dir zipper = sink DOWN $ moveDown  (till moveUp isHoriSplit zipper) dir
  where isHoriSplit wZip =
          case getWindow wZip of
            (Split Vert _ _) -> True
            _                -> False

moveHorizontal dir zipper = sink DOWN $ moveDown  (till moveUp isHoriSplit zipper) dir
  where isHoriSplit wZip =
          case getWindow wZip of
            (Split Hori _ _) -> True
            _                -> False
-----------------------------------------------------------------------------------------
till :: (WindowZip a -> WindowZip a) -> (WindowZip a -> Bool) -> WindowZip a -> WindowZip a
till f test wZip
    | test wZip = wZip
    | otherwise = till f test $ f wZip
    
-- | write this in a clearer way
split :: WindowZip a -> SplitType -> a -> WindowZip a
split (Root w)       splitType a = Zip (Root $ Split splitType w undefined) (Leaf a) DOWN 
split (Zip wz w dir) splitType a = Zip (Zip wz (Split splitType w undefined) dir) (Leaf a) DOWN
          
getWindow (Zip  _ w _) = w
getWindow (Root w)     = w

setWindow (Root _) w    = Root w
setWindow (Zip a _ c) w = Zip a w c


getAtWindow wZip = case getWindow wZip of
  (Leaf a) -> Just a
  _        -> Nothing

getParentZip (Root a)         = Root a
getParentZip (Zip parent _ _) = parent

getDir (Root _) = UP
getDir (Zip _ _ d) = d


join Leaf{} _ _ = error "Zip bad state: "
join (Split t u d) insertee dir =
  case dir of
       UP   -> Split t insertee d
       DOWN -> Split t u insertee
