module Main where

import Data.Maybe
import Text.Parsec
import System.Environment

import AST
import VC
import Parser.Program

main :: IO ()
main = do
    putStrLn "verify-c"
    textToParse <- fmap listToMaybe getArgs
    case textToParse of 
        Nothing -> putStrLn "please provide an argument"
        (Just t) -> do
            src <- readFile t
            let ast = parse program "" src
            putStrLn $ case ast of
                (Right (Program (f : _))) -> "VCs: " ++ show (verify f)
                (Right (Program _)) -> "error: something went wrong while parsing"
                (Left err) -> "error: " ++ show err
        


