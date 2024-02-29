module LexerSpec (spec) where

import Test.Hspec

import Lexer

spec :: Spec
spec = do
    describe "scan" $ do
        describe "when the lexeme is a constant" $ do
            it "tokenizes the digit" $ do
                scan "10" `shouldBe` [DIGIT 10]
            
            it "tokenizes the boolean" $ do
                scan "true" `shouldBe` [T]
                scan "false" `shouldBe` [F]

        it "tokenizes the symbols" $ do
            scan "if" `shouldBe` [IF]
            scan "then" `shouldBe` [THEN]
            scan "else" `shouldBe` [ELSE]
            scan "("  `shouldBe` [LPAREN]
            scan ")"  `shouldBe` [RPAREN]
            scan "succ"  `shouldBe` [SUCC]
            scan "pred"  `shouldBe` [PRED]
            scan ";"  `shouldBe` [SEMICOLON]

