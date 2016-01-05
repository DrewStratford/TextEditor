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
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  (loop textBuffer)

loop :: TextBuffer -> IO ()
loop text = do
  input <- getChar
  clearScreen 
  cursPos 0 0 
  case input of
    '\DEL'     -> do
                  t <- return $ backspace text
                  (x, y) <- return $ getLineCol t
                  drawSection t 0 0 40 40
                  cursPos x y
                  loop t
    '\n'       -> do
      ---- fix this to split over lines
                  t <- return $ newline text
                  (x, y) <- return $ getLineCol t
                  drawSection t 0 0 40 40
                  cursPos x y
                  loop t
    'L'        -> do
                  (x, y) <- return $ getLineCol text
                  t <- return $ moveRight text
                  drawSection t 0 0 40 40
                  (x, y) <- return $ getLineCol text
                  cursPos x y
                  loop t
    'K'        -> do
                  (x, y) <- return $ getLineCol text
                  t <- return $ text `moveLine` (x -1)
                  drawSection t 0 0 40 40
                  (x, y) <- return $ getLineCol text
                  cursPos x y
                  loop t
    'J'        -> do
                  (x, y) <- return $ getLineCol text
                  t <- return $ text `moveLine` (x +1)
                  drawSection t 0 0 40 40
                  (x, y) <- return $ getLineCol text
                  cursPos x y
                  loop t
    'H'        -> do
                  (x, y) <- return $ getLineCol text
                  t <- return $ moveLeft text
                  drawSection t 0 0 40 40
                  (x, y) <- return $ getLineCol text
                  cursPos x y
                  loop t
                  
    'Q'          -> return ()
    c            -> do
                  t <- return $ text `insert` c
                  (x, y) <- return $ getLineCol t
                  drawSection t 0 0 40 40
                  cursPos x y
                  loop t
                    
drawSection :: TextBuffer -> Int -> Int -> Int -> Int -> IO ()
drawSection text x y width height = do
  let lines = getLineSection y height text
  mapM_ ((\cs -> mapM_ putChar cs >> print '\n' ) . toList . S.take width . S.drop x) lines

cursPos :: Int -> Int -> IO ()
cursPos l c = putStr $ "\ESC[" ++ show l ++ ";" ++ show c ++ "H"
{-
-}
