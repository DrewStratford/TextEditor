{-# LANGUAGE PartialTypeSignatures #-}

import Data.Foldable
    
import System.IO 
import System.Console.ANSI
import qualified Data.Sequence as S

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
    
import Data.TextBuffer
    
main :: IO ()
main = do
  file <- readFile "src/Main.hs"
  textBuffer <- return $ fromStrings $ lines file
  
  window <- initScr
  echo False
  --cBreak True
  --intrFlush False
  initCurses
  (loop window textBuffer)
  endWin

loop :: Window -> TextBuffer -> IO ()
loop window text = do
  --wclear window 
  (l,c) <- return $ getLineCol text
  (scrLine, scrCol) <- scrSize
  wMove window (scrLine -1) 0
  drawLine 40 $ "<" ++ show l ++ ":" ++ show c ++ ">"
  wMove window 0 0 
  drawSection window  text 0 0 (scrCol-1) (scrLine -1)
  wMove window l c
  wRefresh window
  
  input <- getCh

  wMove window (scrLine -1) (min (scrCol -10) 40)
  drawLine 40 $ show input
  wRefresh window

  case input of
    (KeyChar '\ESC')    -> do
                           let (x,y)  = getLineCol text
                           (x',y')   <- selectLoop window text
                           let t = removeSection x y x' y' text
                           loop window t
    (KeyChar '\DEL')    -> do
                           let t = backspace text
                           loop window t
    (KeyChar '\n')      -> do
                           t <- return $ newline text
                           loop window t
    (KeyChar 'L')       -> do
                           (x, y) <- return $ getLineCol text
                           t <- return $ text `moveCol` (y+1)
                           loop window t
    (KeyChar 'K')       -> do
                           (x, y) <- return $ getLineCol text
                           t <- return $ text `moveLine` (x -1)
                           loop window t
    (KeyChar 'J')       -> do
                           (x, y) <- return $ getLineCol text
                           t <- return $ text `moveLine` (x +1)
                           loop window t
    (KeyChar 'H')       -> do
                           (x, y) <- return $ getLineCol text
                           t <- return $ text `moveCol` (y-1)
                           loop window t
                           
    (KeyChar 'Q')         -> return ()
    (KeyChar c  )         -> do
                          let t = text `insert` c
                          loop window t
    _                     -> loop window text
                  

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
