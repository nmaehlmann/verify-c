module VerificationOptions where
import Options

data VerificationOptions = VerifyOptions
    { hasColor :: Bool
    , smtTimeout :: Int
    , noSkip :: Bool
    }

instance Options VerificationOptions where
    defineOptions = pure VerifyOptions
        <*> simpleOption "color" False
            "Whether or not to use ANSI colors."
        <*> simpleOption "timeout" 5
            "SMT solver timeout in seconds."
        <*> simpleOption "no-skip" False
            "Whether or not to continue verification after a condition could not be verified."