module Eval (eval) where

import Parser 

data ErrorEvaluation = NoRuleApplies deriving Show

eval1 :: Term -> Either ErrorEvaluation Term 
eval1 t = case t of
    TmIf TmTrue t2 t3 -> Right t2
    TmIf TmFalse t2 t3 -> Right t3
    TmIf ti t2 t3 -> eval1 ti
    TmSucc t1 -> eval1 t1
    TmPred TmZero -> Right TmZero
    TmPred (TmSucc t) | isNumericVal t -> Right t
    TmPred t1 -> eval1 t1
    TmIsZero TmZero -> Right TmTrue
    TmIsZero t | isNumericVal t ->  Right t
    TmIsZero t1 -> eval1 t1
    _ -> Left NoRuleApplies

eval :: Term -> Term
eval t = case eval1 t of
    Left NoRuleApplies -> t 
    Right term -> term
