module IT.MaxSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec hiding (Empty)

import AST
import Parser.Statement

maxSpec :: IO Spec
maxSpec = do
    maxSrc <- readFile "examples/max.c0" 
    return $ do
        describe "Parser.Statement" $ do
            it "parses a program that finds the maximal element of an array" $ do
                (parse statement "" maxSrc) `shouldBe` maxExp

a = LIdt $ Idt "a"                
i = LIdt $ Idt "i"
r = LIdt $ Idt "r"
n = LIdt $ Idt "n"                
maxExp = Right (Seq (Seq assn0ToI assn0ToR) (While iLessN (Seq (ITE aAtRLessAAtI assnIToR Empty) assnIPlus1ToI)))
assn0ToI = Assignment i (ALit 0)
assn0ToR = Assignment r (ALit 0)
iLessN = BComp Less (AIdt i) (AIdt n)
aAtRLessAAtI = BComp Less (AIdt (LArray a (AIdt r))) (AIdt (LArray a (AIdt i)))
assnIToR = Assignment r (AIdt i)
assnIPlus1ToI = Assignment i (ABinExp Add (AIdt i) (ALit 1))
