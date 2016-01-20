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
  , window  :: Window
  }
  
type TextM a= StateT TextDisplay IO a

toText :: (TextBuffer -> TextBuffer) -> TextM ()
toText f = do
  t <- get
  let text' = f $ text t
  put t{ text = text' }
  return ()

output :: TextM()
output = do
  td <- get
  let (l,c) = getLineCol $ text td
      tLine = topLine td
  (height, width) <- lift scrSize
  lift $  do wMove (window td) 0 0 
             drawSection (window td) (text td) (0 + tLine) 0 (width-1) (height-1)
             wMove (window td) l c 
             wRefresh $ window td

-----------------------------------------------------------------------

main :: IO ()
main = do
  file <- readFile "src/Main.hs"
  textBuffer <- return $ fromStrings $ lines file
  
  window <- initScr
  echo False
  initCurses
  let textDisplay = TextDisplay textBuffer 0 window
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
                  

selectLoop :: Window -> TextBuffer -> IO (Int, Int)
selectLoop window text = do

  (l,c) <- return $ getLineCol text
  (scrLine, scrCol) <- scrSize
  wMove window (scrLine -1) 0
  drawLine 40 $ "<" ++ show l ++ ":" ++ show c ++ ">"
  wMove window 0 0 
  drawSection window  text 0 0 (scrCol-1) (scrLine -1)
  wMove window l c
  wRefresh window

  input <- getCh

  case input of
    (KeyChar '\ESC') -> return $ getLineCol text
  
    (KeyChar 'L')    -> do
                        (x, y) <- return $ getLineCol text
                        t <- return $ text `moveCol` (y+1)
                        selectLoop window t
    (KeyChar 'K')    -> do
                        (x, y) <- return $ getLineCol text
                        t <- return $ text `moveLine` (x -1)
                        selectLoop window t
    (KeyChar 'J')    -> do
                        (x, y) <- return $ getLineCol text
                        t <- return $ text `moveLine` (x +1)
                        selectLoop window t
    (KeyChar 'H')    -> do
                        (x, y) <- return $ getLineCol text
                        t <- return $ text `moveCol` (y-1)
                        selectLoop window t

    _                -> selectLoop window text

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

{-
-}
