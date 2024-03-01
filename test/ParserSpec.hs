module ParserSpec (spec) where

import Test.Hspec
import Parser

spec :: Spec
spec = do
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