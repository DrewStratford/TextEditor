{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor, RankNTypes, ExistentialQuantification #-}
module Control.TextMonad
  ( emptyTextState
  , TextState
  , TextT

  , getKey
  , doEffect
  , open
  , close
  , next
  , previous
  , insertText
  , insertChar
  , insertString
  , deleteAmount
  , moveColumn
  , moveToEOL
  , moveToSOL
  , setColumn
  
  , flush
  , drawText

  , outputAsLines
  , run
  ) where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Data.Foldable

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.IO

import qualified Graphics.Vty as Vty

import qualified Zip.Zip as Z

import qualified Data.Buffer as B

{-
  TO CONSIDER:
  We may be able better modularise the display commands
  (e.g. Vsplit etc.) as a parametric value similar to
  effect
-}
data InputRequest = Wait Int | Block
data TextF effect next =
    Get (Vty.Event -> next)
  | Put String next
  | forall a. Effect (effect a) (a -> next)
  -- opening and swapping between buffers
  | Open String next
  | Close next
  | Swap String next
  -- Handles splits and the movement between them
  | Vsplit next
  | MvLeft next
  | MvRight next

instance Functor (TextF e) where
  fmap f (Get g)        = Get (f . g)
  fmap f (Put str n)    = Put str (f n)
  fmap f (Effect eff n) = Effect eff (f . n)
  fmap f (Open str n)   = Open str (f n)
  fmap f (Close n)      = Close (f n)
  fmap f (Swap str n)   = Swap str (f n)
  fmap f (Vsplit n)     = Vsplit (f n)
  fmap f (MvLeft n)     = MvLeft (f n)
  fmap f (MvRight n)    = MvRight (f n)

  
data TextState = TextState
  { text :: B.Buffer
  , name :: String
  , col :: Int
  , vty :: Vty.Vty
  , picture :: Vty.Image
  }

emptyTextState :: Vty.Vty -> String -> TextState
emptyTextState vty str = TextState mempty str 0 vty Vty.emptyImage


-- | A monad transformer for controlling the text state
type TextT m = FreeT (TextF m) (StateT TextState m)


{-
  Just a bunch of simple getters and setters
-}
modifyText f = lift $ modify' fs
  where fs ts = ts { text = f (text ts) }


moveColumn amount = lift $ modify' fs
  where fs ts = ts { col = col' ts amount } 
        col' ts amount = min (end ts) (max 0 (col ts + amount))
        end = B.size . text

moveToEOL :: Monad m => TextT m ()
moveToEOL = whileM_ (not <$> atEOL) (moveColumn 1)

moveToSOL :: Monad m => TextT m ()
moveToSOL = whileM_ (not <$> atSOL) (moveColumn (-1))


setColumn c = lift $ modify' fs
  where fs ts = ts { col = c }

getCol :: Monad m => TextT m Int
getCol = do
  state <- lift get
  return $ col state


atEOL :: Monad m => TextT m Bool
atEOL = do
  buffer <- text <$> lift get
  c <- getCol
  return (B.atEOL c buffer || B.atEnd c buffer)

atSOL :: Monad m => TextT m Bool
atSOL = do
  buffer <- text <$> lift get
  c <- getCol
  return (B.atEOL c buffer || B.atEnd c buffer)
  

  
  

insertText :: Monad m => T.Text -> TextT m ()
insertText = insertBuffer . B.fromText



insertChar :: Monad m => Char -> TextT m ()
insertChar = insertBuffer . B.fromText . T.singleton

  
insertString :: Monad m => String -> TextT m ()
insertString = insertBuffer . B.fromText . T.pack

insertBuffer :: Monad m => B.Buffer -> TextT m ()
insertBuffer buffer = do
  let amount = B.size buffer
  column <- getCol
  modifyText (B.insertAt column buffer)
  moveColumn amount

deleteAmount :: Monad m => Int -> TextT m ()
deleteAmount amount = do
  column <- getCol
  modifyText (B.deleteAmountAt amount column)
  moveColumn (-amount)
  

--------------------------------------------------------------------------------
-- Some helpers
getKey :: Monad m => TextT m Vty.Event
getKey = liftF (Get id)

  
puts :: String -> Monad m => TextT m ()
puts str = liftF (Put str ())

open :: String -> Monad m => TextT m ()
open str = liftF (Open str ())

close :: Monad m => TextT m ()
close = liftF (Close ()) 

next :: Monad m => TextT m ()
next = liftF (MvRight ())

previous :: Monad m => TextT m ()
previous = liftF (MvLeft ())

  
doEffect :: Monad m => m a -> TextT m a
doEffect eff = liftF (Effect eff id)

flush :: TextT IO ()
flush = do
  state <- lift get
  let vty' = vty state
      img  = picture state
      pic  = Vty.picForImage img
  doEffect $ Vty.update vty' pic

outputAsLines :: TextT IO ()
outputAsLines = do
  state <- lift get
  doEffect (mapM_ (print) $ take 80 $ B.toLines $ text state)

drawText :: Monad m => TextT m ()
drawText = do
  state  <- lift get

  let buffer = B.insertAt columns (B.fromString "^") $ text state
      columns = col state
      header = Vty.string Vty.defAttr (name state)
      img = drawSection buffer 80 -- TODO: Make width configurable

  lift $ put (state {picture = header Vty.<-> img})
  
drawLine :: Int -> B.Buffer -> Vty.Image
drawLine width buffer = Vty.text' Vty.defAttr $ B.toText paddedBuffer
  where paddedBuffer = B.take width $ B.dropEnd 1 buffer
        
--  TextBuffer to draw from, x, y, width of section, height of section
drawSection :: B.Buffer -> Int -> Vty.Image
drawSection text width =
  Vty.vertCat $ map (drawLine 80) (B.toLines text)

  
padString :: Int -> String -> String
padString indent = concatMap expand
  where expand '\t' = [' ' | _ <- [0 .. indent]]
        expand c    = [c]
--------------------------------------------------------------------------------
-- The interpreter
  {-
     At the moment this is pretty basic interpreter
     That allows certain IO actions to be performed
  -}

type TextIO = TextT IO ()

  
{- |
    Interprets the TextT free monad and turns it into a StateT.
    It also takes a zipper of buffers which is used to store the open
    buffers. The current buffer should always be in atPoint, and this
    will need to be replaced when swapping buffers.
-}
interpret :: Z.Zip TextState -> TextT IO a -> StateT TextState IO a
interpret buffers text = do
  command <- runFreeT text
  case command of

    Free (Get n) -> do
      vty <- vty <$> get
      key <- liftIO $ Vty.nextEvent vty
      interpret buffers (n key)

    Free (Put str n) -> do
      liftIO $ putStrLn str
      interpret buffers n

    Free (Effect eff n) -> do
      e <- liftIO eff
      interpret buffers (n e)
      
    Free (Open str n) -> do
      state <- get
      let vty' = vty state
          newBuffer  = emptyTextState vty' str
          buffers'   = state `Z.replace` buffers 
          buffers''  = newBuffer `Z.insert` buffers'
      put newBuffer
      interpret buffers'' n

    Free (Close n) -> do
      let buffers' = Z.delete buffers
      put (Z.getPoint buffers')
      interpret buffers' n

    Free (Swap str n) -> do
      --TODO
      interpret buffers n

    Free (Vsplit n) -> do
      --TODO
      interpret buffers n

    Free (MvLeft n) -> do
      buffers' <- buffPrev buffers
      interpret buffers' n

    Free (MvRight n) -> do
      buffers' <- buffNext buffers
      interpret buffers' n

    Pure a -> return a

-- helpers for MvRight and MvLeft
buffNext :: Z.Zip TextState -> StateT TextState IO (Z.Zip TextState)
buffNext buffers = do
  text <- get
  let buffers' = Z.right (text `Z.replace` buffers)
  put (Z.getPoint buffers')
  return buffers'
  
buffPrev :: Z.Zip TextState -> StateT TextState IO (Z.Zip TextState)
buffPrev buffers = do
  text <- get
  let buffers' = Z.left (text `Z.replace` buffers)
  put (Z.getPoint buffers')
  return buffers'

run :: TextT IO a -> TextState -> IO a
run textT state =
  let buffers = Z.zipper state
      stateT  = interpret buffers textT
  in fst <$> runStateT stateT state

