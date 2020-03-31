module IT.SwapSpec where
import Test.Hspec
import Text.Parsec
import Parser.BooleanExpression
import Parser.Program
import AST
import VC

swapSpec :: IO Spec
swapSpec = do
    swapSrc <- readFile "examples/swap.c0" 
    return $ do
        describe "VC.verify" $ do
            let (Right swapProgram) = parse program "" swapSrc
            let swapImplicationTxt = "*y == Y && *x == X"
            let (Right swapImplication) = parse bExpFO "" swapImplicationTxt
            let [Just (VC _ (BBinExp Implies _ implication))] = map vcUnliftMemory $ verifyProgram swapProgram
            it "generates a truthy implication for the swap program" $ do
                implication `shouldBe` swapImplication