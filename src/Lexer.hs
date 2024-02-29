module Lexer
    ( Token(..)
    , scan
    ) where

import Data.Char
import Data.Word

data Token 
    = T
    | F
    | IF
    | THEN
    | ELSE
    | ZERO
    | SUCC
    | PRED
    | ISZERO
    | DIGIT Word8
    | RPAREN
    | LPAREN
    | SEMICOLON
    deriving ( Show, Eq )

scan ::  String -> [Token]
scan [] = []
scan ('t':'r':'u':'e':xs) = T: scan xs
scan ('f':'a':'l':'s':'e':xs) = F: scan xs
scan ('i':'f':xs) = IF: scan xs
scan ('t':'h':'e':'n':xs) = THEN: scan xs
scan ('e':'l':'s':'e':xs) = ELSE: scan xs
scan ('0':xs) = ZERO: scan xs
scan ('s':'u':'c':'c':xs) = SUCC: scan xs
scan ('p':'r':'e':'d':xs) = PRED: scan xs
scan ('i':'s':'z':'e':'r':'o':xs) = ISZERO: scan xs
scan ('(':xs) = LPAREN: scan xs
scan (')':xs) = RPAREN: scan xs
scan (';':xs) = SEMICOLON: scan xs
scan (char:xs) | isDigit char = tokenizerDigit (char:xs)
               | isSpace char = scan xs 
               | otherwise    = error "Incorrect syntax, symbol not identified"

tokenizerDigit ::  String -> [Token]
tokenizerDigit s = 
    case reads s :: [(Word8, String)] of
        [(n, xs)] -> DIGIT n : scan xs
        _ -> error "Incorrect syntax: Number cannot be parsed"
    

