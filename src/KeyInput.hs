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
         | KeyCharSeq String
         | KeyUp
         | KeyDown
         | KeyLeft
         | KeyRight
         | KeyDelete
         | KeyBackspace
         | KeyPageUp
         | KeyPageDown
         | KeyEsc
         | KeyEnter
         | KeyTab
         | Blank
         deriving (Show, Eq, Ord)

getKey :: IO Key
getKey = do
  _ <- hWaitForInput stdin (-1)
  maybeKey <- tillEmpty >>= \x -> return $ walkTrie x inputTrie
  return $ fromMaybe Blank maybeKey

tillEmpty :: IO String
tillEmpty = do
  cont <- hReady stdin
  if cont
     then do
          c <- getChar
          fmap (c:) tillEmpty
     else return ""

inputTrie = fromList $ specialKeys ++ keyChars ++ [("jk", KeyCharSeq "jk")]

keyChars :: [(String, Key)]
keyChars = [([c], KeyChar c) | c <- [' ' .. '~']]
           
specialKeys =  [( "\ESC[3~", KeyDelete), ( "\ESC[B", KeyDown ),
                ( "\ESC[C", KeyRight), ( "\ESC[D", KeyLeft ),
                ( "\ESC[A", KeyUp ), ("\ESC", KeyEsc),
                ("\DEL", KeyBackspace), ("\ESC[5~", KeyPageUp),
                ("\ESC[6~", KeyPageDown), ("\r", KeyEnter), ("\n", KeyEnter), ("\t", KeyTab)]
