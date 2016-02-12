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



data Trie b  = Trie (M.Map Char (Maybe b, Trie b)) deriving (Eq, Show)

empty :: Trie b
empty = Trie M.empty

insert :: String -> a -> Trie a -> Trie a
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
        
fromList :: [(String, v)] -> Trie v
fromList = foldl' (\trie (s,v) -> insert s v trie) empty

---------------------------------------------------------------------------------------------------
{-
   viewing and interacting with the trie
-}
walkTrie :: String -> Trie a -> Maybe a
walkTrie [] _ = Nothing
walkTrie [c] trie =  getValue trie c
walkTrie (c:cs) (Trie map) =  join $ fmap (walkTrie cs . snd) (M.lookup c map)

stepTrie :: Char -> Trie a -> Maybe (Trie a)
stepTrie c (Trie map) =  fmap snd  (M.lookup c map)

getValue :: Trie a -> Char -> Maybe a 
getValue (Trie map) c = join $ fst <$> M.lookup c map


test :: IO (Maybe Char)
test = do
  c <- getChar
  case c of
    't' -> return Nothing
    _   -> return $ Just c


-----------------------------------------------------------------------------------------------------
