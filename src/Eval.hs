module Eval where

import Parser

data Value = B Bool | N Int | Undefined
    deriving Show


eval :: Term -> Value
eval t = 
    case t of 
        T -> B True
        F -> B False 
        Zero -> N 0
        Succ t' -> 
            case eval t' of 
                N k -> N (k + 1)
                _ -> Undefined
        Pred t' ->
            case eval t' of 
                N k -> N (k - 1)
                _ -> Undefined
        IsZero t' ->
            case eval t' of 
               N 0 -> B True
               N _ -> B False 
               _ -> Undefined
        IfExpr t1 t2 t3 ->
            case eval t1 of 
                B True -> eval t2
                B False -> eval t3
                _ -> Undefined         
