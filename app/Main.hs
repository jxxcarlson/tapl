import           System.IO  
import           Control.Monad (unless)
import           System.IO

import Prelude hiding(any)

main :: IO()
main =
  do
    putStrLn "\nWelcome to TAPL"
    repl
   


repl :: IO()
repl =
  do
    putStr (" > " ) >> hFlush stdout  
    line <- getLine
    putStrLn ("   Echo: " ++ line)
    repl
