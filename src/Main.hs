{-# LANGUAGE PartialTypeSignatures #-}

import Data.Foldable
import Control.Monad.State
    
import System.IO 
import System.Console.ANSI
import qualified Data.Sequence as S

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
    
import Data.TextBuffer

-----------------------------------------------------------------------

data TextDisplay = TextDisplay
  { text    :: TextBuffer
  , topLine :: Int
  , leftCol :: Int
  , window  :: Window
  }
  
type TextM a = StateT TextDisplay IO a

toText :: (TextBuffer -> TextBuffer) -> TextM ()
toText f = do
  t <- get
  let text' = f $ text t
  put t{ text = text' }
  return ()

output :: TextM()
output = do
  scrollScreen
  td <- get
  let (l,c) = getLineCol $ text td
      tLine = topLine td
      lCol  = leftCol td
  (height, width) <- lift scrSize
  lift $  do wMove (window td) 0 0 
             drawSection (window td) (text td) lCol tLine (width-1) (height-1)
             wMove (window td) (l - tLine) (c - lCol)
             wRefresh $ window td

-- | scrolls the screen based on cursor position
scrollScreen :: TextM ()
scrollScreen = do
  td <- get
  let (cursLine, cursCol) = getLineCol $ text td
      tLine               = topLine td
      lCol                = leftCol td

  (scrLines, scrCols) <- lift scrSize

  let tLine'
       | cursLine > tLine + scrLines - 1 = tLine + (cursLine - (tLine +scrLines - 1))
       | otherwise = min cursLine tLine
      lCol'
       | cursCol > lCol + scrCols - 1 = lCol + (cursCol - (lCol + scrCols - 1))
       | otherwise = min cursCol lCol
  
  put td { topLine = tLine', leftCol = lCol'}
  return ()

  
-----------------------------------------------------------------------

main :: IO ()
main = do
  file <- readFile "src/Main.hs"
  textBuffer <- return $ fromStrings $ lines file
  
  window <- initScr
  echo False
  initCurses
  let textDisplay = TextDisplay textBuffer 0 0 window
  evalStateT loop textDisplay
  endWin

loop :: TextM ()
loop = do
  --wclear window 
  output
  input <- lift getCh
  case input of
    (KeyChar '\ESC')    -> loop 
    (KeyChar 'P')       -> loop
                           --let t = text `insertSection` fromStrings ["!test of!"]
                           --loop window t
    (KeyChar '\DEL')    -> do
                           toText backspace
                           loop
    (KeyChar '\n')      -> do
                           toText newline
                           loop
    (KeyChar 'L')       -> do
                           toText moveLeft
                           loop
    (KeyChar 'K')       -> do
                           toText moveUp 
                           loop
    (KeyChar 'J')       -> do
                           toText moveDown 
                           loop
    (KeyChar 'H')       -> do
                           toText moveRight
                           loop
                           
    (KeyChar 'Q')         -> return ()
    (KeyChar c  )         -> do
                          toText (`insert` c)
                          loop 
    _                     -> loop 
                  


-- | Draws the section of the specified TextBuffer onto the curses window
--  curses window, TextBuffer to draw from, x, y, width of section, height of section
drawSection :: Window -> TextBuffer -> Int -> Int -> Int -> Int -> IO ()
drawSection window text x y width height = do
  let lines :: [String]
      lines =  toList $ fmap toList (getLineSection y height text)
      go [] = return ()
      go (s:ss) = do
        (drawLine width . drop x) s
        drawLine 1 "\n"
        go ss
  go lines

