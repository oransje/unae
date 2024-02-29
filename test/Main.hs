module Main 
    (main
    ) where


import Test.Hspec
import qualified Spec

main :: IO ()
main = do
    hspec Spec.spec
