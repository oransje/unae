module ParserSpec (spec) where

import Test.Hspec

import Parser
import Lexer(Token(..))

spec :: Spec
spec = do
    describe "parse" $ do
        describe "when tokens are values" $ do
            describe "with boolean values" $ do
                it "parses T to TmTrue" $ do
                    parse [T] `shouldBe` TmTrue

                it "parses F to TmFalse" $ do
                    parse [F] `shouldBe` TmFalse

            describe "with numerical values" $ do
                it "parses ZERO to TmZero" $ do
                    parse [ZERO] `shouldBe` TmZero

                it "parses 2 in TmSucc terms" $ do
                    parse [DIGIT 2] `shouldBe` TmSucc (TmSucc TmZero)

        describe "when tokens are conditionals" $ do
            it "parses if block to TmIf TmTerm TmTerm TmTerm block" $ do
                let tokens = [IF, T, THEN, ZERO, ELSE, F]
                parse tokens `shouldBe` TmIf TmTrue TmZero TmFalse

            describe "with nested conditioning" $ do
                it "parses nested if-then-else" $ do
                    let tokens = [IF, T, THEN, IF, F, THEN, ZERO, ELSE, T, ELSE, F]
                    parse tokens `shouldBe` TmIf TmTrue (TmIf TmFalse TmZero TmTrue) TmFalse

        describe "when the expression is predecessor" $ do
            it "parses the structure of predecessor in TmSucc pattern" $ do
                parse [PRED, DIGIT 2] `shouldBe` TmPred (TmSucc (TmSucc TmZero))

        describe "when the function is isZero?" $ do
            it "parses the structure of term isZero" $ do
                parse [ISZERO, DIGIT 0] `shouldBe` TmIsZero TmZero

        
    describe "isNumericVal" $ do
        it "asserts term zero as numeric" $ do
            isNumericVal TmZero `shouldBe` True

        it "asserts successors term of TmZero as numeric" $ do
            let foo = TmSucc $ TmSucc TmZero

            isNumericVal foo `shouldBe` True

    describe "isVal" $ do
        it "asserts term TmTrue" $ do
            isVal TmTrue `shouldBe` True
        
        it "asserts term TmFalse" $ do
            isVal TmFalse `shouldBe` True

        it "asserts numericality terms are Terms" $ do
            isVal TmZero `shouldBe` True

        it "does not recognize expression as values" $ do
            let iftest = TmIf TmTrue TmFalse TmTrue
            let tmiszero = TmIsZero TmZero
            let tmpred = TmPred TmFalse

            isVal iftest `shouldBe` False
            isVal tmiszero `shouldBe` False
            isVal tmpred `shouldBe` False