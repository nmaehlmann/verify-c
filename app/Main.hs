module Main where
import Control.Exception
import Control.Monad
import System.Directory
import System.Process hiding (env)
import System.IO.Error
import System.Console.Pretty
import System.FilePath
import System.Environment
import Data.Maybe
import Data.List
import Data.Char
import Text.Parsec
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

clearTempPaths :: IO ()
clearTempPaths = do
    removeIfExists vcPath
    removeIfExists z3InputPath
    removeIfExists z3OutputPath

main :: IO ()
main = do
    textToParse <- fmap listToMaybe getArgs
    case textToParse of 
        Nothing -> putStrLn "error: input file was not specified."
        (Just path) -> do
            
            -- load source and environment
            src <- readFile path
            let envPath = addExtension (fst (splitExtension path)) "env"
            envExists <- doesFileExist envPath
            env <- if envExists then readFile envPath else return ""

            let ast = parse program "" src
            case ast of
                (Right prog) -> do
                    let vcs = verifyProgram prog
                    let vcInfos = map printVC vcs
                    let maxLen = maximum $ map length vcInfos
                    let paddedVCInfos = map (\s -> s ++ ": ") $ map (padR ' ' maxLen) vcInfos
                    let maxRank = length vcs
                    let rankedVCInfos = zipWith (\r s -> printRank r maxRank ++ s) [1..] paddedVCInfos
                    
                    putStrLn $ "Generated " ++ show (length vcs) ++ " verification condition(s). Starting proof:"
                    
                    result <- verifyVCs env VOk $ zip rankedVCInfos vcs
                    case result of
                        VOk -> printVerificationOK
                        VError -> printVerificationFailed
                
                (Left err) -> do
                    putStrLn "parser error:"
                    putStrLn $ show err

printVerificationOK :: IO ()
printVerificationOK = do
    inColor <- supportsPretty
    putStrLn $ withBGColor Green "VERIFICATION OK" inColor

printVerificationFailed :: IO ()
printVerificationFailed = do
    inColor <- supportsPretty
    putStrLn $ withBGColor Red "VERIFICATION FAILED" inColor

        
printVC :: VC a -> String
printVC (VC vcInfo _) = printVCInfo vcInfo

printVCInfo :: VCInfo -> String
printVCInfo (CWhileTrue line) = "While Case True" ++ showLine line
printVCInfo (CWhileFalse line) = "While Case False" ++ showLine line
printVCInfo (CAssertion line) = "Assertion" ++ showLine line
printVCInfo (CFunCall (Idt name) line) = "Call Function " ++ name ++ showLine line
printVCInfo (CPrecondition (Idt name)) = "Precondition " ++ name

showLine :: LineNo -> String
showLine (LineNo l) = " (l:" ++ show l ++ ") "

printRank :: Int -> Int -> String
printRank pcurrent pmax = "[" ++ spadded ++ "/" ++ smax ++ "] "
    where 
        smax = show pmax
        scurrent = show pcurrent
        spadded = padL '0' (length smax) scurrent

data VerificationStatus = Verified | Violated | SimplifyFailed | Timeout | SMTError | Skipped

data VerificationResult = VError | VOk

verifyVCs :: String -> VerificationResult -> [(String, VC Refs)] -> IO VerificationResult
verifyVCs _ s [] = return s
verifyVCs env VError ((description, _) : vcs) = printVCResult description Skipped >> verifyVCs env VError vcs
verifyVCs env VOk ((description, vc@(VC _ refsFO)) : vcs) = clearTempPaths >> do
    case vcUnliftMemory vc of

        Just (VC _ plainFO) -> do
            writeFile vcPath $ show plainFO
            let smt = toSMT env plainFO
            writeFile z3InputPath smt
            (_, z3Result, z3Err) <- readProcessWithExitCode "z3" [z3InputPath, "-T:5"] ""
            writeFile z3OutputPath z3Result

            let trimmedZ3Result = trim z3Result
            case trimmedZ3Result of
                "unsat" -> printVCResult description Verified >> verifyVCs env VOk vcs
                "sat" -> printVCResult description Violated >> verifyVCs env VError vcs
                "timeout" -> printVCResult description Timeout >> verifyVCs env VError vcs
                _ -> do 
                    printVCResult description SMTError
                    _ <- verifyVCs env VError vcs
                    when (not (null z3Result)) $ putStrLn z3Result
                    when (not (null z3Err)) $ putStrLn z3Result
                    return VError

        Nothing -> do
            writeFile vcPath $ show refsFO
            printVCResult description SimplifyFailed
            verifyVCs env VError vcs

printVCResult :: String -> VerificationStatus -> IO ()
printVCResult description status = do
    inColor <- supportsPretty
    putStrLn $ description ++ printStatus status inColor

printStatus :: VerificationStatus -> Bool -> String
printStatus Verified        = withBGColor Green   "OK"
printStatus Violated        = withBGColor Red     "VIOLATED"
printStatus SimplifyFailed  = withBGColor Red     "SIMPLIFY FAILED"
printStatus Timeout         = withBGColor Red     "TIMEOUT"
printStatus SMTError        = withBGColor Red     "SMT ERROR"
printStatus Skipped         = withBGColor Yellow  "SKIPPED"

withBGColor :: Color -> String -> Bool -> String
withBGColor c s True = bgColor c $ " " ++ s ++ " "
withBGColor _ s False =  " " ++ s ++ " "

trim:: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

padL :: a -> Int -> [a] -> [a]
padL p s l
    | length l >= s = l
    | otherwise     = replicate (s - length l) p ++ l

padR :: a -> Int -> [a] -> [a]
padR p s l = take s $ l ++ repeat p