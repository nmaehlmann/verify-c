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

n = LIdt $ Idt "n"                
p = LIdt $ Idt "p"
c = LIdt $ Idt "c"                
fac = Right (Seq (Seq assn1ToP assn1ToC) (While cLessEqN inv (Seq assnPMulCToP assnCPlus1ToC)))
assn1ToP = Assignment p (ALit 1)
assn1ToC = Assignment c (ALit 1)
cLessEqN = BComp LessOrEqual (AIdt c) (AIdt n)
inv = FOComp Equal (AIdt p) (AFunCall (Idt "fac") [ABinExp Sub (AIdt c) (ALit 1)])
assnPMulCToP = Assignment p (ABinExp Mul (AIdt p) (AIdt c))
assnCPlus1ToC = Assignment c (ABinExp Add (AIdt c) (ALit 1))