module LexerSpec (spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Lexer

spec :: Spec
spec = do
    describe "scan" $ do
        describe "lexemes" $ do 
            describe "when the lexeme is a constant" $ do
                it "tokenizes the digit" $ do
                    scan "10" `shouldBe` [DIGIT 10]
                
                it "tokenizes the boolean" $ do
                    scan "true" `shouldBe` [T]
                    scan "false" `shouldBe` [F]

            it "tokenizes the symbols" $ do
                scan "0" `shouldBe` [ZERO]
                scan "if" `shouldBe` [IF]
                scan "then" `shouldBe` [THEN]
                scan "else" `shouldBe` [ELSE]
                scan "("  `shouldBe` [LPAREN]
                scan ")"  `shouldBe` [RPAREN]
                scan "succ"  `shouldBe` [SUCC]
                scan "pred"  `shouldBe` [PRED]
                scan ";"  `shouldBe` [SEMICOLON]

        describe "expressions" $ do
            it "tokenize the entire expression" $ do
                scan "succ (pred 0);" `shouldBe` [SUCC, LPAREN, PRED, ZERO, RPAREN, SEMICOLON]
                scan "iszero (pred (succ (succ 0)));" `shouldBe` [ISZERO, LPAREN, PRED, LPAREN, SUCC, LPAREN, SUCC, ZERO, RPAREN, RPAREN, RPAREN, SEMICOLON]
                scan "if false then true else false;" `shouldBe` [IF, F, THEN, T, ELSE, F, SEMICOLON]

            it "empty case" $ do 
                scan "" `shouldBe` []
                scan "      " `shouldBe` []
        
        describe "when does not tokenize" $ do
                describe "when is not a recognized symbol" $ do
                    it "throws an error" $ do
                         evaluate (scan "+-") `shouldThrow` anyErrorCall
                describe "when the lexe is not a digit" $ do
                    it "throws an error" $ do
                         evaluate (scan "flamengooo") `shouldThrow` anyErrorCall
