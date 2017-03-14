{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Control.TextMonad
  ( emptyTextState
  , TextT

  , getKey
  , open
  , close
  , next
  , previous
  , insertText
  , insertChar
  , insertString
  , backspaceAmount
  , deleteAmount
  , moveColumn
  , moveLine
  , moveRight
  , moveLeft
  , moveToEOL
  , moveToSOL
  
  , flush
  , drawText
  , saveFile

  , outputAsLines
  , run
  
  , module Lens.Micro.Mtl
  , point
  , screenHeight
  , screenWidth
  ) where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Data.Foldable
import Data.IORef

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

import qualified Graphics.Vty as Vty
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

import qualified Data.Buffer as B
import qualified Zip.Zip as Z


{-
  TO CONSIDER:
  We may be able better modularise the display commands
  (e.g. Vsplit etc.) as a parametric value 
-}
data InputRequest = Wait Int | Block
data TextF next =
    Get (Vty.Event -> next)
  | Put String next
  -- opening and swapping between buffers
  | Open String next
  | Close next
  | Swap String next
  -- Handles splits and the movement between them
  | Vsplit next
  | MvLeft next
  | MvRight next
  --
  | ToBuffer (B.Buffer -> B.Buffer) next
  | GetBuffer (B.Buffer -> next)
  deriving (Functor)

  
data TextState = TextState
  { _text :: IORef B.Buffer
  , _name :: String
  , _point :: Int
  , _preferredCol :: Maybe Int -- used for more complex line movements
  , _topScrollLine :: Int
  , _tabSize :: Int
  , _vty :: Vty.Vty -- Ideally this will be moved out
  , _picture :: Vty.Image
  , _unsavedChanges :: Bool
  , _screenHeight :: Int
  , _screenWidth :: Int
  } 

makeLenses ''TextState

emptyTextState :: Vty.Vty -> String -> (IORef B.Buffer) -> TextState
emptyTextState vty str ref = TextState ref str 0 Nothing 0 8 vty Vty.emptyImage False 0 0


-- | A monad transformer for controlling the text state
type TextT m = FreeT TextF (StateT TextState m)


{-
  Just a bunch of simple getters and setters
-}

bufferSize :: Monad m => TextT m Int
bufferSize = do
  b <- getBuffer
  return $ B.size b


onBuffer :: Monad m => (B.Buffer -> b) -> TextT m b
onBuffer f = f <$> getBuffer


moveColumn :: Monad m => Int -> TextT m ()
moveColumn amount = do
  modifying point (\x -> max 0 (x + amount))
  preferredCol .= Nothing


moveLine :: Monad m => Int -> TextT m ()
moveLine amount = do
  prefCol <- use preferredCol
  (r, c) <- getCursor
  buff   <- getBuffer
  tabs <- use tabSize

  -- if first line move in a while, we must set preferredCol
  when (isNothing prefCol) $ do
    assign preferredCol (Just c)
  Just col <- use preferredCol -- This seems hacky, though, it should always work due to the above when

  let newPoint = B.cursorToPoint tabs (targetLine, col) buff
      targetLine = min end $ max 0 (r + amount)
      end = B.lineCount buff

  if targetLine < 0 || targetLine >= end
    then return ()
    else point .= newPoint
  

moveNInLine :: Monad m => Int -> TextT m ()
moveNInLine n = replicateM_ n $ do
  whileM_ (not <$> atEOL) (moveColumn 1)

moveLeft :: Monad m => TextT m ()
moveLeft = do b <- atSOL; unless b (moveColumn (-1))

moveRight :: Monad m => TextT m ()
moveRight = do b <- atEOL; unless b (moveColumn 1)

moveToEOL :: Monad m => TextT m ()
moveToEOL = whileM_ (not <$> atEOL) (moveColumn 1)

moveToSOL :: Monad m => TextT m ()
moveToSOL = whileM_ (not <$> atSOL) (moveColumn (-1))

scrollLine :: Monad m => TextT m ()
scrollLine = do
  curScrollLine <- use topScrollLine
  height        <- use screenHeight
  (l, c) <- getCursor
  let deltaUp   = l - curScrollLine
      deltaDown = l - (curScrollLine + (height - 1))
  if l < curScrollLine
    then topScrollLine %= (+deltaUp) --assign topScrollLine (curScrollLine + deltaUp)
    else if l > (curScrollLine + height - 1)
    then topScrollLine %= (+deltaDown)
    else return ()


getCursor :: Monad m => TextT m (Int, Int)
getCursor = do
  p        <- use point
  tabWidth <- use tabSize
  onBuffer (B.pointToCursor p tabWidth)


atEOL :: Monad m => TextT m Bool
atEOL = do
  c <- use point
  orM [ onBuffer $ B.atEOL c
      , onBuffer $ B.atEnd c 
      ]

atSOL :: Monad m => TextT m Bool
atSOL = do
  c <- use point
  orM [ onBuffer $ B.atEOL (c - 1) 
      , return (c == 0)
      ]

-- Insertions into the buffer. At their root they are casts and calls to insertBuffer
insertText :: Monad m => T.Text -> TextT m ()
insertText = insertBuffer . B.fromText

insertChar :: Monad m => Char -> TextT m ()
insertChar = insertBuffer . B.fromText . T.singleton

insertString :: Monad m => String -> TextT m ()
insertString = insertBuffer . B.fromText . T.pack

insertBuffer :: Monad m => B.Buffer -> TextT m ()
insertBuffer buffer = do
  let amount = B.size buffer
  column <- use point
  toBuffer (B.insertAt column buffer)
  moveColumn amount


backspaceAmount :: Monad m => Int -> TextT m ()
backspaceAmount amount = do
  column <- use point
  toBuffer (B.deleteAmountAt amount column)
  moveColumn (-amount)
  

deleteAmount :: Monad m => Int -> TextT m ()
deleteAmount amount = do
  column <- use point
  toBuffer (B.deleteAmountAt amount (column + amount))
  moveColumn 0

--------------------------------------------------------------------------------
-- Some helpers
getKey :: Monad m => TextT m Vty.Event
getKey = liftF (Get id)
  

open :: Monad m => String -> TextT m ()
open str = liftF (Open str ())

close :: Monad m => TextT m ()
close = liftF (Close ()) 

next :: Monad m => TextT m ()
next = liftF (MvRight ())

previous :: Monad m => TextT m ()
previous = liftF (MvLeft ())

toBuffer :: Monad m => (B.Buffer -> B.Buffer) -> TextT m ()
toBuffer f = liftF (ToBuffer f ())

getBuffer :: Monad m => TextT m B.Buffer
getBuffer  = liftF (GetBuffer id)
  
--------------------------------------------------------------------------------

flush :: TextT IO ()
flush = do
  vty'    <- use vty
  (r, c)  <- getCursor
  topLine <- use topScrollLine
  img     <- use picture
  
  let curs = Vty.Cursor c (r - topLine)
      pic  = Vty.picForImage img
  liftIO $ Vty.update vty' pic { Vty.picCursor = curs} 


outputAsLines :: TextT IO ()
outputAsLines = do
  text <- getBuffer
  liftIO (mapM_ (print) $ take 80 $ B.toLines $ text)


drawText :: Monad m => TextT m ()
drawText = do
  text   <- getBuffer
  scrollLine
  topLine <- use topScrollLine
  vty <- use vty

  width  <- use screenWidth
  height <- use screenHeight

  columns <- use point
  name    <- use name
  tabSize <- use tabSize

  let buffer = B.takeLines height $ B.dropLines topLine text
      header = Vty.string Vty.defAttr name
      img = drawSection buffer width tabSize 
      curs = Vty.string Vty.defAttr $ show $ B.pointToCursor columns tabSize text
      pref = Vty.string Vty.defAttr $ show $ B.measure text

  --assign picture (header Vty.<-> img Vty.<-> (curs Vty.<|> pref))
  assign picture img 

  
drawLine :: Int -> Int -> B.Buffer -> Vty.Image
drawLine width indent buffer = Vty.string Vty.defAttr paddedBuffer
  -- this could be made easier to read
  where paddedBuffer = B.expandTabs indent $ T.unpack $ B.toText $ B.take width line
        line = if B.lineCount buffer > 0 then B.dropEnd 1 buffer else buffer
        
--  TextBuffer to draw from, x, y, width of section, height of section
drawSection :: B.Buffer -> Int -> Int ->  Vty.Image
drawSection text width indent =
  Vty.vertCat $ map (drawLine width indent) (B.toLines text)

  
  
--------------------------------------------------------------------------------


saveFile :: TextT IO ()
saveFile = do
  name <- use name 
  asText <- B.toText <$> getBuffer
  liftIO $ T.writeFile (name ++ "!") asText
  return ()

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
interpret buffers textState = do
  command <- runFreeT textState
  case command of

    Free (Get n) -> do
      vty <- use vty
      key <- liftIO $ Vty.nextEvent vty
      interpret buffers (n key)

    Free (Put str n) -> do
      liftIO $ putStrLn str
      interpret buffers n

    Free (Open str n) -> do
      vty <- use vty
      state <- get
      bufRef <- liftIO $ newIORef mempty
      let newBuffer  = emptyTextState vty str bufRef
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

    Free (ToBuffer f n) -> do
      ref <- use text 
      liftIO $ modifyIORef' ref f
      interpret buffers n

    Free (GetBuffer n) -> do
      ref <- use text 
      buff <- liftIO $ readIORef ref 
      interpret buffers (n buff)

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

