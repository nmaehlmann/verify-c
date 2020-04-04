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
import Data.List.Utils (replace)

type App = ExceptT VerificationStatus (ReaderT Settings IO)

data Settings = VerifyCSettings 
    { smtEnvironment :: String
    , hasColor :: Bool
    , smtTimeout :: Int
    , noSkip :: Bool
    }

data VerificationStatus = Verified | Violated | SimplifyFailed | SMTExportFailed | Timeout | SMTError String String | Skipped

data VerificationResult = VError | VOk

type Environment = String

data Rank = Rank Int Int
data Description = Description Rank String

targetPath :: FilePath
targetPath = "target"

clearTempPaths :: IO ()
clearTempPaths = do
    dirExists <- doesDirectoryExist targetPath
    when dirExists $ removeDirectoryRecursive targetPath
    createDirectory targetPath

runApp :: Settings -> App a -> IO ()
runApp settings app = void $ runReaderT (runExceptT app) settings

exceptMaybe :: VerificationStatus -> Maybe a -> App a
exceptMaybe s m = except $ maybeToRight s m

main :: IO ()
main = Options.runCommand $ \opts args -> do
    clearTempPaths
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
                    let rankedVCInfos = zipWith (\r s -> Description (Rank r maxRank) s) [1..] paddedVCInfos
                    
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

printDescription :: Description -> String
printDescription (Description rank s) = printRank rank ++ ": " ++ s

descriptionToFile :: Description -> String -> FilePath
descriptionToFile description extension = targetPath ++ "/" ++ (descriptionToFilename description) ++ "." ++ extension

descriptionToFilename :: Description -> String
descriptionToFilename (Description rank s) = rankToFilePath rank ++ "_" ++ identifier
    where identifier = 
            replace " " "_" $
            trim $
            replace "(" "" $
            replace ")" "" $
            replace ":" ""  $
            s

printRank :: Rank -> String
printRank (Rank pCurrent pMax) = "[" ++ sPadded ++ "/" ++ sMax ++ "] "
    where 
        sMax = show pMax
        sCurrent = show pCurrent
        sPadded = padL '0' (length sMax) sCurrent

rankToFilePath :: Rank -> String
rankToFilePath (Rank pCurrent pMax) = sPadded ++ "_of_" ++ sMax
    where 
        sMax = show pMax
        sCurrent = show pCurrent
        sPadded = padL '0' (length sMax) sCurrent


verifyVCs :: [(Description, VC Refs)] -> App VerificationResult
verifyVCs [] = return VOk
verifyVCs (vc@(description, _) : vcs) = do
    nextFun <- catchE
        (verifyVC vc >> printVCResult description Verified >> return verifyVCs)
        (\errorStatus -> (printVCResult description errorStatus) >> return skipVCs)
    nextFun vcs

skipVCs :: [(Description, VC Refs)] -> App VerificationResult
skipVCs [] = return VError
skipVCs allVCs@((description, _) : vcs) = do
    skip <- not <$> noSkip <$> ask
    if skip 
        then printVCResult description Skipped >> skipVCs vcs
        else verifyVCs allVCs
    return VError

verifyVC :: (Description, VC Refs) -> App ()
verifyVC (description, vc@(VC _ refsFO)) =  do

    let writeWithExtension ext str = runIO (writeFile (descriptionToFile description ext) str)

    runIO (hFlush stdout)
    writeWithExtension "refs" $ show refsFO

    (VC _ plainFO) <- exceptMaybe SimplifyFailed $ unliftMemory vc
    writeWithExtension "plain" $ show plainFO

    env <- smtEnvironment <$> ask
    smt <- exceptMaybe SMTExportFailed $ SMT.export env plainFO
    let z3InputPath = descriptionToFile description "z3"
    runIO $ writeFile z3InputPath smt

    timeout <- smtTimeout <$> ask
    (_, z3Result, z3Err) <- runIO $ readProcessWithExitCode "z3" [z3InputPath, "-T:" ++ show timeout] ""
    writeWithExtension "z3_log" z3Result

    let trimmedZ3Result = trim z3Result
    case trimmedZ3Result of
        "unsat"     -> return ()
        "sat"       -> throwE Violated
        "timeout"   -> throwE Timeout
        _           -> throwE $ SMTError z3Result z3Err 

printVCResult :: Description -> VerificationStatus -> App ()
printVCResult description status = do
    inColor <- hasColor <$> ask
    runIO $ putStrLn $ printDescription description ++ printStatus status inColor
    case status of
        (SMTError z3Result z3Err) -> do
            when (not (null z3Result))  $ runIO $ putStrLn z3Result
            when (not (null z3Err))     $ runIO $ putStrLn z3Result
        _ -> return ()

printStatus :: VerificationStatus -> Bool -> String
printStatus Verified         = withColor Green   "OK"
printStatus Violated         = withColor Red     "VIOLATED"
printStatus SimplifyFailed   = withColor Red     "SIMPLIFY FAILED"
printStatus SMTExportFailed  = withColor Red     "SMT_EXPORT FAILED"
printStatus Timeout          = withColor Red     "TIMEOUT"
printStatus (SMTError _ _)   = withColor Red     "SMT ERROR"
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