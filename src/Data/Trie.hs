{-# LANGUAGE DeriveFunctor #-}

module Data.Trie
       ( empty
       , insert
       , Trie
       , walkTrie
       , interactiveWalkTrie
       , fromList
       ) where

import Data.List hiding (insert)
import Data.Maybe
import qualified Data.Map.Strict as M
import Control.Monad.Trans.Maybe
import Control.Monad



data Trie b  = Trie (M.Map Char (Maybe b, Trie b)) deriving (Functor, Eq, Show)

empty = Trie M.empty

insert :: String -> a -> Trie a -> Trie a
insert [] _ _ = error "cannot map value to empty string in Trie"
insert [c] v (Trie t) = Trie $ M.insert c (Just v, Trie M.empty) t
insert (c:cs) v trie@(Trie t) 
    | c `M.member` t = Trie $ M.insert c (oldV,step) t
    | otherwise = Trie $ M.insert c subtree t
  where subtree = (Nothing, insert cs v (Trie M.empty))
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


interactiveWalkTrie :: IO (Maybe Char) -> Trie a -> IO (Maybe a)
interactiveWalkTrie getInput trie = ioWalkTrie getInput trie Nothing

ioWalkTrie :: IO (Maybe Char) -> Trie a -> Maybe a -> IO (Maybe a)
ioWalkTrie getInput trie prev = do
  input <- getInput
  case input of
    Nothing -> return prev
    (Just c') -> do 
                let trie' = stepTrie c' trie
                    prev' = getValue trie c'
                case trie' of
                  Nothing -> return Nothing
                  (Just t) -> ioWalkTrie getInput t prev'
                
getValue :: Trie a -> Char -> Maybe a 
getValue (Trie map) c = join $ fst <$> M.lookup c map


test :: IO (Maybe Char)
test = do
  c <- getChar
  case c of
    't' -> return Nothing
    _   -> return $ Just c
