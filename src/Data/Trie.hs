module Data.Trie
       ( empty
       , insert
       , Trie
       , walkTrie
       , fromList
       ) where

import Data.List hiding (insert)
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Maybe
import Control.Monad



newtype Trie c b  = Trie (M.Map c (Maybe b, Trie c b)) deriving (Eq, Show)

empty :: Trie c b
empty = Trie M.empty


insert :: Ord c => [c] -> a -> Trie c a -> Trie c a
insert [] _ _ = empty
insert [c] v (Trie t) = Trie $ M.insertWith merge c (Just v, Trie M.empty) t
  -- ensures the subtree of potentially existing trees are not deleted. (left is new trie)
  where merge (v, Trie m) (_, Trie m2) = (v, Trie $ M.union m m2)
insert (c:cs) v trie@(Trie t) 
    | c `M.member` t = Trie $ M.insert c (oldV,step) t
    | otherwise      = Trie $ M.insert c subtree t
  where subtree = (Nothing, insert cs v empty)
        step    = maybe trie (insert cs v) $ stepTrie c trie
        oldV    = getValue trie c
        

fromList :: Ord c => [([c], v)] -> Trie c v
fromList = foldl' (\trie (s,v) -> insert s v trie) empty

---------------------------------------------------------------------------------------------------
{-
   viewing and interacting with the trie
-}
walkTrie :: Ord c => [c] -> Trie c a -> Maybe a
walkTrie [] _ = Nothing
walkTrie [c] trie =  getValue trie c
walkTrie (c:cs) (Trie map) =  join $ fmap (walkTrie cs . snd) (M.lookup c map)

stepTrie :: Ord c => c -> Trie c a -> Maybe (Trie c a)
stepTrie c (Trie map) =  fmap snd  (M.lookup c map)

getValue :: Ord c => Trie c a -> c -> Maybe a 
getValue (Trie map) c = join $ fst <$> M.lookup c map


test :: IO (Maybe Char)
test = do
  c <- getChar
  case c of
    't' -> return Nothing
    _   -> return $ Just c


-----------------------------------------------------------------------------------------------------

collapse :: Trie c a -> [c] -> Trie [c] a
collapse (Trie trie) cs = undefined


-----------------------------------------------------------------------------------------------------

data Trie' a = Same a (Trie' a)
             | Split (M.Map a (Trie' a))
             | End
             deriving Show

newTrie' = foldr id End . map Same

complete :: Ord a => [a] -> Trie' a -> Maybe (Trie' a)
complete [] t = Just t
complete (a:as) (Same a' sub) = whenPlus (a == a') (complete as sub)
complete (a:as) (Split subs) = do
  node <- a `M.lookup` subs
  complete as node

  
walk :: Ord a => [a] -> Trie' a -> ([a], Trie' a)
walk [] t = ([], t)
walk str@(a:as) trie@(Same a' sub)
  | a == a' = (walk as sub)
  | otherwise = (str, trie)
walk str@(a:as) trie@(Split subs) = fromMaybe (str, trie) (walk as <$> node) 
  where node = a `M.lookup` subs

whenPlus test cont = if test then cont else mzero


getAll :: Ord a => Trie' a -> [a] -> [[ a]]
getAll End acc = return acc
getAll (Same a subs) acc = getAll subs (acc ++ [a])
getAll node acc = do
  node' <- asNodes node
  getAll node' acc

asNodes node = case node of
  Split subs -> do
    (k,v) <- M.toList subs
    return $ Same k v
  other -> return other

trie' = Same 'a' (Split $ M.fromList
                  [
                     ('b', (Split $ M.fromList [( 'c', End), ('d', End)])),
                     ('z', End)
                  ]
                 )
