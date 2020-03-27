module Main where
import System.Directory
import System.Process hiding (env)
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
import Control.Monad

vcPath :: FilePath
vcPath = "vcs.txt"

z3InputPath :: FilePath
z3InputPath = "vcs.z3"

z3EnvPath :: FilePath
z3EnvPath = "env.z3"

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
            env <- readFile z3EnvPath
            let ast = parse program "" src
            case ast of
                (Right prog) -> case (verifyProgram prog) of
                    (Right plain) -> verifyVCs env plain
                        -- do
                        
                        -- writeFile vcPath $ show plain
                        
                        -- let conjunction = foldl1 (BBinExp And) plain
                        -- let smt = toSMT conjunction
                        -- writeFile z3InputPath smt

                        -- z3Result <- readProcess "z3" [z3InputPath] ""
                        -- writeFile z3OutputPath z3Result

                        -- let trimmedZ3Result = trim z3Result
                        -- case trimmedZ3Result of
                        --     "unsat" -> putStrLn "verification conditions successfully proven."
                        --     "sat" -> putStrLn "verification conditions are violated."
                        --     _ -> putStrLn "unknown Z3 error: " >> putStrLn z3Result

                    (Left stateful) -> do
                        writeFile vcPath $ show stateful
                        putStrLn "could not reduce verification conditions:"
                        putStrLn (show stateful)
                
                (Left err) -> do
                    putStrLn "parser error:"
                    putStrLn (show err)
        

verifyVCs :: String -> [BExp FO Plain] -> IO ()
verifyVCs _ [] = return ()
verifyVCs env (fo:fos) = do

    writeFile vcPath $ show fo

    let smt = toSMT env fo
    writeFile z3InputPath smt

    (_, z3Result, z3Err) <- readProcessWithExitCode "z3" [z3InputPath] ""
    writeFile z3OutputPath z3Result

    let trimmedZ3Result = trim z3Result
    case trimmedZ3Result of
        "unsat" -> putStrLn "verification condition successfully proven." >> verifyVCs env fos
        "sat" -> putStrLn "verification condition is violated."
        _ -> do 
            putStrLn "unknown Z3 error: "
            when (not (null z3Result)) $ putStrLn z3Result
            when (not (null z3Err)) $ putStrLn z3Result

trim:: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
