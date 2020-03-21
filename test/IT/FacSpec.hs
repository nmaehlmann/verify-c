module IT.FacSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.Statement

facSpec :: IO Spec
facSpec = do
    facSrc <- readFile "examples/fac.c0" 
    return $ do
        describe "Parser.Statement" $ do
            it "parses a faculty program" $ do
                (parse statement "" facSrc) `shouldBe` fac

mkIdt = AIdt
n = LIdt $ Idt "n"                
p = LIdt $ Idt "p"
c = LIdt $ Idt "c"
fac = Right (Seq (Seq assn1ToP assn1ToC) (While cLessEqN inv (Seq assnPMulCToP assnCPlus1ToC)))
assn1ToP = Assignment p (ALit 1)
assn1ToC = Assignment c (ALit 1)
cLessEqN = BComp LessOrEqual (mkIdt c) (mkIdt n)
inv = BComp Equal (mkIdt p) (AFunCall (Idt "fac") [ABinExp Sub (mkIdt c) (ALit 1)])
assnPMulCToP = Assignment p (ABinExp Mul (mkIdt p) (mkIdt c))
assnCPlus1ToC = Assignment c (ABinExp Add (mkIdt c) (ALit 1))