{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Control.TextMonad where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Map as M
import System.IO

import Data.TextBuffer
import Graphics.Vty(Event(..), Key(..), Modifier(..))

data InputRequest = Wait Int | Block
data TextF effect next =
    Get (Event -> next)
  | Put String next
  | Effect (forall a. effect a -> next)
  | ScreenSize ((Int,Int) -> next)
  deriving (Functor)

data TextState = TextState
  { text :: TextBuffer
  , scrollLine :: Int
  , scrollCol  :: Int
  , line :: Int
  , col  :: Int
  }

-- | A monad transformer for controlling the text state
type TextT m = FreeT (TextF m) (StateT TextState m)


modifyText f = lift $ modify' fs
  where fs ts = ts { text = f (text ts) }

  
