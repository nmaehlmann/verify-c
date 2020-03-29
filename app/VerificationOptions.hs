module VerificationOptions where
import Options

data VerificationOptions = VerifyOptions
    { hasColor :: Bool
    }

instance Options VerificationOptions where
    defineOptions = pure VerifyOptions
        <*> simpleOption "color" False
            "Whether or not to use ANSI colors."