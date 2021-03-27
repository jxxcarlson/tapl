module Parser where

import qualified MiniParsec as MP
import Prelude hiding(succ, pred)

type Parser a = MP.MPParser Char MP.ParseError a

data Term
    = T
    | F
    | Zero
    | Succ Term
    | Pred Term
    | IsZero Term
    | IfExpr Term Term Term
    deriving Show



term = MP.choice "yada" [trueParser, falseParser, zero, succ, pred, isZero, ifExpr]
-- runParser trueParser "true"
trueParser :: Parser Term
trueParser =  (\_ -> T) <$> string' "true" 

falseParser :: Parser Term
falseParser = (\_ -> F) <$> string' "false"

zero :: Parser Term
zero = (\_ -> Zero) <$> string' "0"

succ :: Parser Term
succ = (\t -> Succ t) <$> (string' "succ" *> term)

pred :: Parser Term
pred = (\t -> Pred t) <$> (string' "pred" *> term)

isZero :: Parser Term
isZero = (\t -> Pred t) <$> (string' "pred" *> term)

ifExpr :: Parser Term
ifExpr  = (\t -> IfExpr t) <$> (string' "if" *> term) <*> (string' "then" *> term) <*> (string' "else" *> term)


-- HELPERS

char :: Char -> Parser Char
char c = MP.satisfy [c]     (== c)

string :: [Char] -> Parser [Char]
string = traverse char

string' :: [Char] -> Parser [Char]
string' str = string str <* spaces


space  = MP.satisfy "space" (\c -> c == ' ')

spaces = MP.many space
