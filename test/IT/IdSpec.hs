module IT.IdSpec where
import Test.Hspec
import Text.Parsec
import Parser.BooleanExpression
import Parser.Program
import AST
import VC
import LiftMemory

idSpec :: IO Spec
idSpec = do
    swapSrc <- readFile "examples/id.c0" 
    return $ do
        describe "VC.verify" $ do
            let (Right swapProgram) = parse program "" swapSrc
            let swapImplicationTxt = "a == a"
            let (Right swapImplication) = bLiftMemory <$> parse bExpFO "" swapImplicationTxt
            let [(BBinExp Implies _ implication)] = verifyProgram swapProgram
            it "generates a truthy implication for the id program" $ do
                implication `shouldBe` swapImplication