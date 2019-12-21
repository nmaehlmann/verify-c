module IT.FacSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec

import AST
import Parser.Statement

facSpec :: IO Spec
facSpec = do
    facSrc <- readFile "test/IT/fac.c0" 
    return $ do
        describe "Parser.Statement" $ do
            it "parses a faculty program" $ do
                (parse statement "" facSrc) `shouldBe` fac

fac = Right (Seq (Seq assn1ToP assn1ToC) (While (BOr cLessN cEqN) (Seq assnPMulCToP assnCPlus1ToC)))
assn1ToP = Assignment (LIdt "p") (ALit 1)
assn1ToC = Assignment (LIdt "c") (ALit 1)
cLessN = BLess (AIdt (LIdt "c")) (AIdt (LIdt "n"))
cEqN = BEq (AIdt (LIdt "c")) (AIdt (LIdt "n"))
assnPMulCToP = Assignment (LIdt "p") (ABinExp Mul (AIdt (LIdt "p")) (AIdt (LIdt "c")))
assnCPlus1ToC = Assignment (LIdt "c") (ABinExp Add (AIdt (LIdt "c")) (ALit 1))