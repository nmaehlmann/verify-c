module IT.IdSpec where
import Test.Hspec
import Text.Parsec
import Parser.BooleanExpression
import Parser.Program
import AST
import VC

spec :: Spec
spec = describe "VC" $ do
    swapSrc <- runIO $ readFile "test/resources/id.c0" 
    let (Right swapProgram) = parse program "" swapSrc
    let swapImplicationTxt = "a == a"
    let (Right swapImplication) = parse bExpFO "" swapImplicationTxt
    let [Just (VC _ (BBinExp Implies _ implication))] = map unliftMemory $ verifyProgram swapProgram
    it "generates a truthy implication for the id program" $ do
        implication `shouldBe` swapImplication