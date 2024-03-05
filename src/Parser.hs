module Parser
    (Term(..)
    , parse
    , isNumericVal
    , isVal) where

import Control.Applicative ((<|>))

import Lexer (Token(..))

data Term 
    = TmTrue 
    | TmFalse 
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term
    deriving ( Show )

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

parse :: [Token] -> Term
parse tokensList = case parseUnit tokensList of
    Just (term, []) -> term
    _ -> error "Cannot parse the expressin."

parseUnit :: [Token] -> Maybe (Term, [Token])
parseUnit tokensList = parseTrue tokensList 
                        <|> parseFalse tokensList 
                        <|> parseIf tokensList
                        <|> parseZero tokensList
                        <|> parseDigit tokensList
                        <|> parsePred tokensList
                        <|> parseIsZero tokensList
                        <|> parseParens tokensList
                        <|> parseSemicolon tokensList


parseTrue :: [Token] -> Maybe (Term, [Token])
parseTrue (T : rest) = Just (TmTrue, rest)
parseTrue _ = Nothing

parseFalse :: [Token] -> Maybe (Term, [Token])
parseFalse (F : rest) = Just (TmFalse, rest)
parseFalse _ = Nothing

parseIf :: [Token] -> Maybe (Term, [Token])
parseIf (IF : rest) = undefined
parseIf (THEN: rest) = undefined
parseIf (ELSE: rest) = undefined

parseZero :: [Token] -> Maybe (Term, [Token])
parseZero (ZERO: rest) = undefined

parseDigit :: [Token] -> Maybe (Term, [Token]) -- TODO: turn all digits in TmSucc in relation to TmZero form
parseDigit (DIGIT d: rest) = undefined

parsePred :: [Token] -> Maybe (Term, [Token])
parsePred (PRED: rest) = undefined

parseIsZero :: [Token] -> Maybe (Term, [Token])
parseIsZero (ISZERO: rest) = undefined

parseParens :: [Token] -> Maybe (Term, [Token])
parseParens (ZERO: rest) = undefined

parseSemicolon :: [Token] -> Maybe (Term, [Token])
parseSemicolon (ZERO: rest) = undefined
