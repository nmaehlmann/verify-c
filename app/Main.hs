module Main where
import System.Directory
import System.Process
import Control.Exception
import System.IO.Error
import Data.Maybe
import Data.List
import Data.Char
import Text.Parsec
import System.Environment
import Parser.Program
import VC
import SMTExport
import AST

vcPath :: FilePath
vcPath = "vcs.txt"

z3InputPath :: FilePath
z3InputPath = "vcs.z3"

z3OutputPath :: FilePath
z3OutputPath = "z3.log"

main :: IO ()
main = do
    textToParse <- fmap listToMaybe getArgs
    case textToParse of 
        Nothing -> putStrLn "error: input file was not specified."
        (Just t) -> do

            removeIfExists vcPath
            removeIfExists z3InputPath
            removeIfExists z3OutputPath

            src <- readFile t
            let ast = parse program "" src
            case ast of
                (Right prog) -> case (verifyProgram prog) of
                    (Right plain) -> do
                        
                        writeFile vcPath $ show plain
                        
                        let conjunction = foldl1 (BBinExp And) plain
                        let smt = toSMT conjunction
                        writeFile z3InputPath smt

                        z3Result <- readProcess "z3" [z3InputPath] ""
                        writeFile z3OutputPath z3Result

                        let trimmedZ3Result = trim z3Result
                        case trimmedZ3Result of
                            "unsat" -> putStrLn "verification conditions successfully proven."
                            "sat" -> putStrLn "verification conditions are violated."
                            _ -> putStrLn "unknown Z3 error: " >> putStrLn z3Result

                    (Left stateful) -> do
                        writeFile vcPath $ show stateful
                        putStrLn "could not reduce verification conditions:"
                        putStrLn (show stateful)
                
                (Left err) -> do
                    putStrLn "parser error:"
                    putStrLn (show err)
        

trim:: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
