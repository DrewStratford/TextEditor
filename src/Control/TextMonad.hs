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
  , moveLine
  , moveRight
  , moveLeft
  , moveToEOL
  , moveToSOL
  , setPoint
  
  , flush
  , drawText
  , saveFile

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
import Data.IORef

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  --
  | ToBuffer (B.Buffer -> B.Buffer) next
  | GetBuffer (B.Buffer -> next)

instance Functor (TextF e) where
  fmap f (Get g)           = Get (f . g)
  fmap f (Put str n)       = Put str (f n)
  fmap f (Effect eff n)    = Effect eff (f . n)
  fmap f (Open str n)      = Open str (f n)
  fmap f (Close n)         = Close (f n)
  fmap f (Swap str n)      = Swap str (f n)
  fmap f (Vsplit n)        = Vsplit (f n)
  fmap f (MvLeft n)        = MvLeft (f n)
  fmap f (MvRight n)       = MvRight (f n)
  fmap f (ToBuffer g next) = ToBuffer g (f next)
  fmap f (GetBuffer g)     = GetBuffer (f . g)

  
data TextState = TextState
  { text :: IORef B.Buffer
  , name :: String
  , point :: Int
  , preferredCol :: Maybe Int -- used for more complex line movements
  , topScrollLine :: Int
  , tabSize :: Int
  , vty :: Vty.Vty -- Ideally this will be moved out
  , picture :: Vty.Image
  , unsavedChanges :: Bool
  } 

emptyTextState :: Vty.Vty -> String -> (IORef B.Buffer) -> TextState
emptyTextState vty str ref = TextState ref str 0 Nothing 0 8 vty Vty.emptyImage False


-- | A monad transformer for controlling the text state
type TextT m = FreeT (TextF m) (StateT TextState m)


{-
  Just a bunch of simple getters and setters
-}

bufferSize :: Monad m => TextT m Int
bufferSize = do
  b <- getBuffer
  return $ B.size b


onBuffer :: Monad m => (B.Buffer -> b) -> TextT m b
onBuffer f = f <$> getBuffer
  

setPreferredCol c = lift $ modify' fs
  where fs ts = ts { preferredCol = c }

getPreferredCol :: Monad m => TextT m (Maybe Int)
getPreferredCol = do
  state <- lift get
  return $ preferredCol state


moveColumn :: Monad m => Int -> TextT m ()
moveColumn amount = do
  end <- bufferSize
  let fs ts = ts { point = point' ts amount } 
      point' ts amount = min end (max 0 (point ts + amount))
  lift $ modify' fs
  -- must reset preferredCol as column has been changed
  setPreferredCol Nothing


moveLine :: Monad m => Int -> TextT m ()
moveLine amount = do
  prefCol <- getPreferredCol
  (r, c) <- getCursor
  buff   <- getBuffer

  -- if first line move in a while, we must set preferredCol
  when (isNothing prefCol) $ do
    setPreferredCol (Just c)
  Just col <- getPreferredCol -- This seems hacky, though, it should always work due to the above when

  let newPoint = B.cursorToPoint (targetLine, col) buff
      targetLine = min end $ max 0 (r + amount)
      end = B.lineCount buff

  if targetLine < 0 || targetLine >= end
    then return ()
    else setPoint newPoint
  

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

scrollLine :: Monad m => Int -> TextT m ()
scrollLine screenHeight = do
  curScrollLine <- getScrollLine
  (l, c) <- getCursor
  let deltaUp   = l - curScrollLine
      deltaDown = l - (curScrollLine + screenHeight)
  if l <= curScrollLine
    then setScrollLine (curScrollLine + deltaUp)
    else if l >= curScrollLine + screenHeight
    then setScrollLine (curScrollLine + deltaDown)
    else return ()

setScrollLine s = lift $ modify' fs
  where fs ts = ts { topScrollLine = s }

getScrollLine :: Monad m => TextT m Int
getScrollLine = do
  state <- lift get
  return $ topScrollLine state

  
setPoint c = lift $ modify' fs
  where fs ts = ts { point = c }

getPoint :: Monad m => TextT m Int
getPoint = do
  state <- lift get
  return $ point state


getCursor :: Monad m => TextT m (Int, Int)
getCursor = do
  p <- getPoint
  state <- lift  get
  let tabWidth = tabSize state
  onBuffer (B.pointToCursor p tabWidth)


atEOL :: Monad m => TextT m Bool
atEOL = do
  c <- getPoint
  orM [ onBuffer $ B.atEOL c
      , onBuffer $ B.atEnd c 
      ]

atSOL :: Monad m => TextT m Bool
atSOL = do
  c <- getPoint
  orM [ onBuffer $ B.atEOL (c - 1) 
      , return (c == 0)
      ]

insertText :: Monad m => T.Text -> TextT m ()
insertText = insertBuffer . B.fromText



insertChar :: Monad m => Char -> TextT m ()
insertChar = insertBuffer . B.fromText . T.singleton

  
insertString :: Monad m => String -> TextT m ()
insertString = insertBuffer . B.fromText . T.pack

insertBuffer :: Monad m => B.Buffer -> TextT m ()
insertBuffer buffer = do
  let amount = B.size buffer
  column <- getPoint
  toBuffer (B.insertAt column buffer)
  moveColumn amount

deleteAmount :: Monad m => Int -> TextT m ()
deleteAmount amount = do
  column <- getPoint
  toBuffer (B.deleteAmountAt amount column)
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

toBuffer :: Monad m => (B.Buffer -> B.Buffer) -> TextT m ()
toBuffer f = liftF (ToBuffer f ())

getBuffer :: Monad m => TextT m B.Buffer
getBuffer  = liftF (GetBuffer id)
  
doEffect :: Monad m => m a -> TextT m a
doEffect eff = liftF (Effect eff id)


--------------------------------------------------------------------------------
flush :: TextT IO ()
flush = do
  state <- lift get
  -- TODO make this tidier
  (width, height) <- doEffect $ Vty.displayBounds $ Vty.outputIface (vty state)
  scrollLine (height - 2)
  (r, c) <- getCursor
  topLine <- getScrollLine
  
  let vty' = vty state
      img  = picture state
      curs = Vty.Cursor c ((r + 1) - topLine)
      pic  = Vty.picForImage img
  doEffect $ Vty.update vty' pic { Vty.picCursor = curs} 

outputAsLines :: TextT IO ()
outputAsLines = do
  text <- getBuffer
  doEffect (mapM_ (print) $ take 80 $ B.toLines $ text)

drawText :: TextT IO ()
drawText = do
  state  <- lift get
  text   <- getBuffer
  topLine <- getScrollLine
  -- TODO make this tidier
  (width, height) <- doEffect $ Vty.displayBounds $ Vty.outputIface (vty state)

  let buffer = B.takeLines (height - 2) $ B.dropLines topLine text
      columns = point state
      header = Vty.string Vty.defAttr (name state)
      img = drawSection buffer width (tabSize state)
      curs = Vty.string Vty.defAttr $ show $ B.pointToCursor columns (tabSize state) text
      pref = Vty.string Vty.defAttr $ show $ B.measure text

  lift $ put (state {picture = header Vty.<-> img Vty.<-> (curs Vty.<|> pref)})
  
-- TO FIX takes one character it shouldn't on the final line.
drawLine :: Int -> Int -> B.Buffer -> Vty.Image
drawLine width indent buffer = Vty.string Vty.defAttr paddedBuffer
  -- this could be made easier to read
  where paddedBuffer = B.expandTabs indent $ T.unpack $ B.toText $ B.take width $ B.dropEnd 1 buffer
        
--  TextBuffer to draw from, x, y, width of section, height of section
drawSection :: B.Buffer -> Int -> Int ->  Vty.Image
drawSection text width indent =
  Vty.vertCat $ map (drawLine 80 indent) (B.toLines text)

  
  
--------------------------------------------------------------------------------


saveFile :: TextT IO ()
saveFile = do
  name <- name <$> lift get
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
      bufRef <- liftIO $ newIORef mempty
      let vty' = vty state
          newBuffer  = emptyTextState vty' str bufRef
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
      ref <- text <$> get
      liftIO $ modifyIORef' ref f
      interpret buffers n

    Free (GetBuffer n) -> do
      ref <- text <$> get
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
