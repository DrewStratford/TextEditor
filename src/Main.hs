import System.IO 
import System.Console.ANSI

import Document
    
main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  hSetEcho stdin False
  update document

update :: Document -> IO ()
update doc = do
  c <- getChar
  case c of
    'L' -> do
           d <- return $ moveRight doc
           (x, y) <- return $ getPos d
           setCursorPosition 39 0
           print $ x 
           setCursorPosition 40 0
           print d
           setCursorPosition x y
           update d
    'H' -> do
           d <- return $ moveLeft doc
           (x, y) <- return $ getPos d
           setCursorPosition 39 0
           print $ x 
           setCursorPosition 40 0
           print d
           setCursorPosition x y
           update d
    'K' -> do
           d <- return $ moveUp doc
           (x, y) <- return $ getPos d
           setCursorPosition 39 0
           print $ x 
           setCursorPosition 40 0
           print d
           setCursorPosition x y
           update d
    'J' -> do
           d <- return $ moveDown doc
           (x, y) <- return $ getPos d
           setCursorPosition 39 0
           print $ x 
           setCursorPosition 40 0
           print d
           setCursorPosition x y
           update d
    _   -> do
           d <- return $ doc `insertChar` c
           clearScreen
           setCursorPosition 0 0
           _ <- output d
           (x, y) <- return $ getPos d
           setCursorPosition 39 0
           print  x
           setCursorPosition 40 0
           print d
           setCursorPosition x y
           update d
