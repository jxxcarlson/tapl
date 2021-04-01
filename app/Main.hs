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
    print parseResult
    case parseResult of 
      ("", Right t) -> print $ Eval.eval t
      (unprocessedInput, Right _) -> putStrLn ("Error, unprocessed input = " ++ unprocessedInput)
      _ -> putStrLn "Something went wrong ... maybe your input could not be parsed."
    repl


-- repl :: IO()
-- repl =
--   do
--     putStr " > " >> hFlush stdout  
--     line <- getLine
--     let parseResult = MiniParsec.runParser Parser.term line
--     case parseResult of 
--       (unprocessedInput, Left _) -> putStrLn "Could not parse input: " ++ unprocessedInput
--       ("", Right t) -> 
--             print $ Eval.eval t
--     repl
