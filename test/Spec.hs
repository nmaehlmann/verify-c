import Test.Hspec

import IT.FacSpec
import IT.AExpSpec
import IT.BExpSpec
import IT.StatementSpec


main :: IO ()
main = do
    specs <- sequence 
        [ aExpSpec
        , bExpSpec
        , statementSpec
        , facSpec
        ]
    hspec $ foldl1 (>>) specs