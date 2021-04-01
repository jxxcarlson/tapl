import           System.IO  
import           Control.Monad (unless)
import           System.IO

import Prelude hiding(any)
import qualified Parser
import qualified MiniParsec
import qualified Eval

main :: IO()
main =
  do
    putStrLn "\nWelcome to TAPL"
    repl

repl :: IO()
repl =
  do
    putStr " > " >> hFlush stdout  
    line <- getLine
    let parseResult = MiniParsec.runParser Parser.term line
    case parseResult of 
      ("", Right t) -> print $ Eval.eval t
      (unprocessedInput, Left _) -> putStrLn ("Could not parse input. Stopped before this: " ++ unprocessedInput)
    repl
