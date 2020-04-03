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
import qualified SMT.Export as SMT
import AST
import qualified Options as Options
import qualified VerificationOptions as VerificationOptions
import Control.Monad.Reader
import System.IO
import Control.Monad.Trans.Except
import Data.Either.Combinators

type App = ExceptT VerificationStatus (ReaderT Settings IO)

data Settings = VerifyCSettings 
    { smtEnvironment :: String
    , hasColor :: Bool
    , smtTimeout :: Int
    , noSkip :: Bool
    }

vcPath :: FilePath
vcPath = "vcs.txt"

z3InputPath :: FilePath
z3InputPath = "vcs.z3"

z3OutputPath :: FilePath
z3OutputPath = "z3.log"

clearTempPaths :: App ()
clearTempPaths = do
    runIO $ removeIfExists vcPath
    runIO $ removeIfExists z3InputPath
    runIO $ removeIfExists z3OutputPath

runApp :: Settings -> App a -> IO ()
runApp settings app = void $ runReaderT (runExceptT app) settings

exceptMaybe :: VerificationStatus -> Maybe a -> App a
exceptMaybe s m = except $ maybeToRight s m

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
                    , noSkip = VerificationOptions.noSkip opts
                    }

            let ast = parse program "" src
            case ast of
                (Right prog) -> runApp settings $ do
                    let vcs = verifyProgram prog
                    let vcInfos = map printVC vcs
                    let maxLen = maximum $ map length vcInfos
                    let paddedVCInfos = map (\s -> s ++ ": ") $ map (padR ' ' maxLen) vcInfos
                    let maxRank = length vcs
                    let rankedVCInfos = zipWith (\r s -> printRank r maxRank ++ s) [1..] paddedVCInfos
                    
                    runIO $ putStrLn $ "Generated " ++ show (length vcs) ++ " verification condition(s). Starting proof:"
                    
                    result <- verifyVCs $ zip rankedVCInfos vcs

                    -- print result
                    runIO $ putStrLn ""
                    case result of
                        VOk -> printVerificationOK
                        VError -> printVerificationFailed
                
                (Left err) -> do
                    putStrLn "parser error:"
                    putStrLn $ show err

printVerificationOK :: App ()
printVerificationOK = do
    inColor <- hasColor <$> ask
    runIO $ putStrLn $ "Summary: " ++ withColor Green "VERIFICATION OK" inColor

printVerificationFailed :: App ()
printVerificationFailed = do
    inColor <- hasColor <$> ask
    runIO $ putStrLn $ "Summary: " ++ withColor Red "VERIFICATION FAILED" inColor

        
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

data VerificationStatus = Verified | Violated | SimplifyFailed | Timeout | SMTError String String | Skipped

data VerificationResult = VError | VOk

type Environment = String

verifyVCs :: [(String, VC Refs)] -> App VerificationResult
verifyVCs [] = return VOk
verifyVCs (vc@(description, _) : vcs) = do
    nextFun <- catchE
        (verifyVC vc >> printVCResult description Verified >> return verifyVCs)
        (\errorStatus -> (printVCResult description errorStatus) >> return skipVCs)
    nextFun vcs

skipVCs :: [(String, VC Refs)] -> App VerificationResult
skipVCs [] = return VError
skipVCs allVCs@((description, _) : vcs) = do
    skip <- not <$> noSkip <$> ask
    if skip 
        then printVCResult description Skipped >> skipVCs vcs
        else verifyVCs allVCs
    return VError

verifyVC :: (String, VC Refs) -> App ()
verifyVC (description, vc@(VC _ refsFO)) = clearTempPaths >> runIO (hFlush stdout) >> do

    runIO $ writeFile vcPath $ show refsFO

    (VC _ plainFO) <- exceptMaybe SimplifyFailed $ unliftMemory vc
    runIO $ writeFile vcPath $ show plainFO

    env <- smtEnvironment <$> ask
    smt <- exceptMaybe SimplifyFailed $ SMT.export env plainFO
    runIO $ writeFile z3InputPath smt

    timeout <- smtTimeout <$> ask
    (_, z3Result, z3Err) <- runIO $ readProcessWithExitCode "z3" [z3InputPath, "-T:" ++ show timeout] ""
    runIO $ writeFile z3OutputPath z3Result

    let trimmedZ3Result = trim z3Result
    case trimmedZ3Result of
        "unsat"     -> return ()
        "sat"       -> throwE Violated
        "timeout"   -> throwE Timeout
        _           -> throwE $ SMTError z3Result z3Err 

printVCResult :: String -> VerificationStatus -> App ()
printVCResult description status = do
    inColor <- hasColor <$> ask
    runIO $ putStrLn $ description ++ printStatus status inColor
    case status of
        (SMTError z3Result z3Err) -> do
            when (not (null z3Result))  $ runIO $ putStrLn z3Result
            when (not (null z3Err))     $ runIO $ putStrLn z3Result
        _ -> return ()

printStatus :: VerificationStatus -> Bool -> String
printStatus Verified         = withColor Green   "OK"
printStatus Violated         = withColor Red     "VIOLATED"
printStatus SimplifyFailed   = withColor Red     "SIMPLIFY FAILED"
printStatus Timeout          = withColor Red     "TIMEOUT"
printStatus (SMTError s1 s2) = withColor Red     "SMT ERROR"
printStatus Skipped          = withColor Default "SKIPPED"

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

runIO :: IO a -> App a
runIO = lift . lift