module Parser where

import Parser

data Term 
    = TmTrue 
    | TmFalse 
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    deriving ( Show )