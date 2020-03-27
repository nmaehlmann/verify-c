module Main where
import Data.Maybe
import Text.Parsec
import System.Environment
import Parser.Program
import VC
import SMTExport
import AST

main :: IO ()
main = do
    putStrLn "verify-c"
    textToParse <- fmap listToMaybe getArgs
    case textToParse of 
        Nothing -> putStrLn "please provide an argument"
        (Just t) -> do
            src <- readFile t
            let ast = parse program "" src
            case ast of
                (Right prog) -> case (verifyProgram prog) of
                    (Right plain) -> do
                        putStrLn $ show plain
                        let conjunction = foldl1 (BBinExp And) plain
                        putStrLn $ bAssert conjunction
                        putStrLn $ "(check-sat)"
                    (Left stateful) -> putStrLn $ "could not simplify: " ++ show stateful
                (Left err) -> putStrLn $ "error: " ++ show err
        


