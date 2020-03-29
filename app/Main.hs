module Main where
import Control.Exception
import Control.Monad
import System.Directory
import System.Process hiding (env)
import System.IO.Error
import System.Console.Pretty
import System.FilePath
import Data.Maybe
import Data.List
import Data.Char
import Text.Parsec
import Parser.Program
import VC
import SMTExport
import AST
import qualified Options as Options
import qualified VerificationOptions as VerificationOptions
import Control.Monad.Reader
import System.IO

type App = ReaderT Settings IO

data Settings = VerifyCSettings { smtEnvironment :: String, hasColor :: Bool, smtTimeout :: Int}

vcPath :: FilePath
vcPath = "vcs.txt"

z3InputPath :: FilePath
z3InputPath = "vcs.z3"

z3OutputPath :: FilePath
z3OutputPath = "z3.log"

clearTempPaths :: App ()
clearTempPaths = do
    lift $ removeIfExists vcPath
    lift $ removeIfExists z3InputPath
    lift $ removeIfExists z3OutputPath

main :: IO ()
main = Options.runCommand $ \opts args -> do
    let textToParse = listToMaybe args
    case textToParse of 
        Nothing -> putStrLn "error: input file was not specified."
        (Just path) -> do
            
            -- load source and environment
            src <- readFile (path :: String)
            let envPath = addExtension (fst (splitExtension path)) "env"
            envExists <- doesFileExist envPath
            env <- if envExists then readFile envPath else return ""

            -- build settings
            let settings = VerifyCSettings 
                    { smtEnvironment = env
                    , hasColor = VerificationOptions.hasColor opts
                    , smtTimeout = VerificationOptions.smtTimeout opts 
                    }

            let ast = parse program "" src
            case ast of
                (Right prog) -> flip runReaderT settings $ do
                    let vcs = verifyProgram prog
                    let vcInfos = map printVC vcs
                    let maxLen = maximum $ map length vcInfos
                    let paddedVCInfos = map (\s -> s ++ ": ") $ map (padR ' ' maxLen) vcInfos
                    let maxRank = length vcs
                    let rankedVCInfos = zipWith (\r s -> printRank r maxRank ++ s) [1..] paddedVCInfos
                    
                    lift $ putStrLn $ "Generated " ++ show (length vcs) ++ " verification condition(s). Starting proof:"
                    
                    result <- verifyVCs env VOk $ zip rankedVCInfos vcs

                    -- print result
                    lift $ putStrLn ""
                    case result of
                        VOk -> printVerificationOK
                        VError -> printVerificationFailed
                
                (Left err) -> do
                    putStrLn "parser error:"
                    putStrLn $ show err

printVerificationOK :: App ()
printVerificationOK = do
    inColor <- hasColor <$> ask
    lift $ putStrLn $ "Summary: " ++ withColor Green "VERIFICATION OK" inColor

printVerificationFailed :: App ()
printVerificationFailed = do
    inColor <- hasColor <$> ask
    lift $ putStrLn $ "Summary: " ++ withColor Red "VERIFICATION FAILED" inColor

        
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

verifyVCs :: String -> VerificationResult -> [(String, VC Refs)] -> App VerificationResult
verifyVCs _ s [] = return s
verifyVCs env VError ((description, _) : vcs) = printVCResult description Skipped >> verifyVCs env VError vcs
verifyVCs env VOk ((description, vc@(VC _ refsFO)) : vcs) = clearTempPaths >> lift (hFlush stdout) >> do
    case vcUnliftMemory vc of

        Just (VC _ plainFO) -> do
            lift $ writeFile vcPath $ show plainFO
            let smt = toSMT env plainFO
            lift $ writeFile z3InputPath smt
            timeout <- smtTimeout <$> ask
            (_, z3Result, z3Err) <- lift $ readProcessWithExitCode "z3" [z3InputPath, "-T:" ++ show timeout] ""
            lift $ writeFile z3OutputPath z3Result

            let trimmedZ3Result = trim z3Result
            case trimmedZ3Result of
                "unsat" -> printVCResult description Verified >> verifyVCs env VOk vcs
                "sat" -> printVCResult description Violated >> verifyVCs env VError vcs
                "timeout" -> printVCResult description Timeout >> verifyVCs env VError vcs
                _ -> do 
                    printVCResult description SMTError
                    _ <- verifyVCs env VError vcs
                    when (not (null z3Result)) $ lift $ putStrLn z3Result
                    when (not (null z3Err)) $ lift $ putStrLn z3Result
                    return VError

        Nothing -> do
            lift $ writeFile vcPath $ show refsFO
            printVCResult description SimplifyFailed
            verifyVCs env VError vcs

printVCResult :: String -> VerificationStatus -> App ()
printVCResult description status = do
    inColor <- hasColor <$> ask
    lift $ putStrLn $ description ++ printStatus status inColor

printStatus :: VerificationStatus -> Bool -> String
printStatus Verified        = withColor Green   "OK"
printStatus Violated        = withColor Red     "VIOLATED"
printStatus SimplifyFailed  = withColor Red     "SIMPLIFY FAILED"
printStatus Timeout         = withColor Red     "TIMEOUT"
printStatus SMTError        = withColor Red     "SMT ERROR"
printStatus Skipped         = withColor Default "SKIPPED"

withColor :: Color -> String -> Bool -> String
withColor c s True = color c $ s
withColor _ s False =  s

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