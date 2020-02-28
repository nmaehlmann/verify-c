import Test.Hspec

import IT.FacSpec
import IT.AExpSpec
import IT.BExpSpec
import IT.FOExpSpec
import IT.LExpSpec
import IT.StatementSpec
import IT.MaxSpec
import IT.FunDefSpec
import IT.VCSpec

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
        ]
    hspec $ foldl1 (>>) specs