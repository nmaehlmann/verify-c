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

fac = Right (Seq (Seq (Assignment (Idt "p") (ALit 1)) (Assignment (Idt "c") (ALit 1))) (While (BOr (BLess (AIdt (Idt "c")) (AIdt (Idt "n"))) (BEq (AIdt (Idt "c")) (AIdt (Idt "n")))) (Seq (Assignment (Idt "p") (ABinExp Mul (AIdt (Idt "p")) (AIdt (Idt "c")))) (Assignment (Idt "c") (ABinExp Add (AIdt (Idt "c")) (ALit 1))))))