import Test.Hspec

import IT.FacSpec
import IT.AExpSpec
import IT.BExpSpec
import IT.FOExpSpec
import IT.LExpSpec
import IT.StatementSpec
import IT.MaxSpec
import IT.FunDefSpec
import IT.TypeSpec
import IT.SimplificationSpec
import IT.LiftMemorySpec
import IT.ReplaceStateSpec
import IT.SwapSpec
import IT.IdSpec

main :: IO ()
main = do
    specs <- sequence 
        [ aExpSpec
        , bExpSpec
        , fOExpSpec
        , lExpSpec
        , statementSpec
        , facSpec
        , maxSpec
        , funDefSpec
        , daggerSpec
        , hashmarkSpec
        , replaceStateSpec
        , typeSpec
        , simplificationSpec
        , simplifyASExpSpec
        , simplifyReadSpec
        , swapSpec
        , idSpec
        ]
    hspec $ foldl1 (>>) specs