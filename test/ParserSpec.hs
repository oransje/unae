module ParserSpec (spec) where

import Test.Hspec

import Parser

spec :: Spec
spec = do
    describe "parse" $ do
        describe "when tokens are values" $ do
            it "parses the units of tokens" $ do
                expectationFailure "Not implemented"

        describe "when tokens are conditions" $ do
            it "parses if-then-else" $ do
                expectationFailure "Not implemented"

            describe " with nested conditioning" $ do
                it "parses nested if-then-else" $ do
                    expectationFailure "Not implemented"

        describe "when the function is succ" $ do
            it "parses the structure of successor" $ do
                expectationFailure "Not implemented"
        
        describe "when the function is pred" $ do
            it "parses the structure of predecessor" $ do
                expectationFailure "Not implemented"
        
        describe "when the function is isZero?" $ do
            it "parses the structure of function isZero" $ do
                expectationFailure "Not implemented"
        
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