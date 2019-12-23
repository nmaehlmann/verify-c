import Test.Hspec

import IT.FacSpec
import IT.AExpSpec
import IT.BExpSpec
import IT.LExpSpec
import IT.StatementSpec
import IT.MaxSpec


main :: IO ()
main = do
    specs <- sequence 
        [ aExpSpec
        , bExpSpec
        , lExpSpec
        , statementSpec
        , facSpec
        , maxSpec
        ]
    hspec $ foldl1 (>>) specs