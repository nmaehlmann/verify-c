module Main where

import Data.Maybe
import Text.Parsec
import System.Environment

import Parser.Program

main :: IO ()
main = do
    putStrLn "verify-c"
    textToParse <- fmap listToMaybe getArgs
    case textToParse of 
        Nothing -> putStrLn "please provide an argument"
        (Just t) -> do
            src <- readFile t
            putStrLn $ show $ parse program "" src
        


