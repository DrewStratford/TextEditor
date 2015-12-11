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
  d <- return $ doc `insertChar` c
  clearScreen
  setCursorPosition 0 0
  _ <- output d
  (x, y) <- return $ getPos d
  setCursorPosition y x
  update d
