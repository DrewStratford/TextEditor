module KeyInput
       ( Key (..)
       , getKey
       , specialKeys
       , keyChars
       , inputTrie
       ) where

import System.IO
import Data.Maybe

import Data.Trie

data Key = KeyChar Char
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyDelete
         | KeyBackspace
         | KeyPageUp
         | KeyPageDown
         | KeyEsc
         | Blank
         deriving (Show, Eq)

getKey :: IO Key
getKey = do
  hWaitForInput stdin (-1)
  maybeKey <- interactiveWalkTrie tillEmpty inputTrie
  return $ fromMaybe Blank maybeKey

tillEmpty :: IO (Maybe Char)
tillEmpty = do
  cont <- hReady stdin
  if cont
     then fmap Just getChar
     else return Nothing

inputTrie = fromList $ specialKeys ++ keyChars

keyChars :: [(String, Key)]
keyChars = [([c], KeyChar c) | c <- [' ' .. '~']]
           
specialKeys = reverse [( "\ESC[3~", KeyDelete), ( "\ESC[B", KeyDown ), ( "\ESC[C", KeyRight), ( "\ESC[D", KeyLeft ), ( "\ESC[A", KeyUp ), ("\ESC", KeyEsc), ("\DEL", KeyBackspace)]
