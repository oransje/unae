module Parser
    (Term(..)
    , parse
    , isNumericVal
    , isVal) where

import Data.Word (Word8)
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
    deriving ( Show, Eq )

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
parseIf (IF : rest) =
    case parseUnit rest of
        Just (cond, THEN : rest') ->
            case parseUnit rest' of
                Just (thenBranch, ELSE : rest'') ->
                    case parseUnit rest'' of
                        Just (elseBranch, rest''') ->
                            Just (TmIf cond thenBranch elseBranch, rest''')
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
parseIf _ = Nothing

parseZero :: [Token] -> Maybe (Term, [Token])
parseZero (ZERO: rest) = Just (TmZero, rest)
parseZero _ = Nothing

parseDigit :: [Token] -> Maybe (Term, [Token])
parseDigit (DIGIT d: rest) = Just (numberInSucc d, rest)
parseDigit _ = Nothing

numberInSucc :: Word8 -> Term
numberInSucc 0 = TmZero
numberInSucc i = TmSucc (numberInSucc (i-1))

parsePred :: [Token] -> Maybe (Term, [Token])
parsePred (PRED : rest) = 
    case parseUnit rest of
        Just (t, rest') -> Just (TmPred t, rest')
        _ -> Nothing
parsePred _ = Nothing

parseIsZero :: [Token] -> Maybe (Term, [Token])
parseIsZero (ISZERO : rest) = 
    case parseUnit rest of
        Just (t, rest') -> Just (TmIsZero t, rest')
        _ -> Nothing
parseIsZero _ = Nothing

parseParens :: [Token] -> Maybe (Term, [Token])
parseParens (LPAREN : rest) = 
    case parseUnit rest of
        Just (t, RPAREN : rest') -> Just (t, rest')
        _ -> Nothing
parseParens _ = Nothing

parseSemicolon :: [Token] -> Maybe (Term, [Token])
parseSemicolon (SEMICOLON : rest) = parseUnit rest
parseSemicolon _ = Nothing
