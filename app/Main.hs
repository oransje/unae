module Main (main) where

import System.IO (hFlush, stdout)

import Lexer
import Parser
import Eval

cursor :: String
cursor = "λarith> "

main :: IO ()
main = do
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn "~~~~~~UNTYPED~~~~~ARITHMETHIC~~~~EXPRESSIONS~~~~~~~~~~~~"
    putStrLn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    putStrLn "Press :q to quit this prompt"
    cli

cli :: IO ()
cli = do
    putStr cursor
    hFlush stdout
    prompt <- getLine
    if prompt == ":q"then 
        putStrLn "Bye Bye"
    else do
        putStrLn $ show $ processArith $ prompt
        cli

processArith :: String -> Term
processArith = eval . parse . scan

