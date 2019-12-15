module Main where

import Data.Maybe
import Text.Parsec
import System.Environment

import AST
import Parser.Statement

main :: IO ()
main = do
    textToParse <- fmap listToMaybe getArgs
    case textToParse of 
        Nothing -> putStrLn "please provide an argument"
        (Just t) -> putStrLn $ show $ parse statement "" t
        


