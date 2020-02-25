module IT.MaxSpec where

import Test.Hspec
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
k = LIdt $ Idt "k"
r = LIdt $ Idt "r"
n = LIdt $ Idt "n"                
maxExp = Right (Seq (Seq assn0ToI assn0ToR) (While iLessN inv (Seq (ITE aAtRLessAAtI assnIToR Empty) assnIPlus1ToI)))
assn0ToI = Assignment i (ALit 0)
assn0ToR = Assignment r (ALit 0)
iLessN = BComp Less (AIdt i) (AIdt n)
inv = Forall (Idt "k" ) (FOBinExp Implies kInRange aAtKLessOrEqR)
kInRange = Predicate (Idt "inRange") [AIdt k, ALit 0, AIdt i]
aAtKLessOrEqR = FOComp LessOrEqual (AIdt (LArray a (AIdt k))) (AIdt r)
aAtRLessAAtI = BComp Less (AIdt (LArray a (AIdt r))) (AIdt (LArray a (AIdt i)))
assnIToR = Assignment r (AIdt i)
assnIPlus1ToI = Assignment i (ABinExp Add (AIdt i) (ALit 1))
