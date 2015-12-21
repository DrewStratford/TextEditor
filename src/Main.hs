import Data.Foldable
    
import System.IO 
import System.Console.ANSI

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
    
import Data.TextBuffer
    
main :: IO ()
main = do
  file <- readFile "src/Main.hs"
  textBuffer <- return $ fromString file
  initCurses
  --raw True
  --cBreak True
  echo False
  window <- initScr
  
  (loop window textBuffer)
  endWin   -- closes everything

loop :: Window -> TextBuffer -> IO ()
loop win text = do
  input <- getch
  move 0 0 
  case decodeKey input of
    KeyChar '\DEL' -> do
                  t <- return $ remove text
                  drawLine 800 (toString t)
                  (x, y) <- return $ getLineCol t
                  move y x
                  wRefresh win
                  loop win t
    KeyEnter     -> do
                  t <- return $ text `insert` '\n'
                  drawLine 800 (toString t)
                  (x, y) <- return $ getLineCol t
                  move y x
                  wRefresh win
                  loop win t
    KeyChar 'Q'  -> return ()
    KeyChar  c   -> do
                  t <- return $ text `insert` c
                  drawLine 800 (toString t)
                  (x, y) <- return $ getLineCol t
                  move y x
                  wRefresh win
                  loop win t
    _            -> loop win text
                    
{-
update :: TextBuffer -> IO ()
update doc = do
  c <- getChar
  case c of
    '\DEL'   -> do
           d <- return $ remove doc
           clearScreen
           setCursorPosition 0 0
           _ <- output d
           hFlush stdout
           update d
    _   -> do
           d <- return $ doc `insert` c
           clearScreen
           setCursorPosition 0 0
           _ <- output d
           hFlush stdout
           update d
-}
