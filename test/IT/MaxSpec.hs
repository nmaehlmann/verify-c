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

mkIdt = AIdt
a = LIdt $ Idt "a"                
i = LIdt $ Idt "i"
k = LIdt $ Idt "k"
r = LIdt $ Idt "r"
n = LIdt $ Idt "n"                
maxExp = Right (Seq (Seq assn0ToI assn0ToR) (While iLessN inv (Seq (ITE aAtRLessAAtI assnIToR Empty) assnIPlus1ToI) (LineNo 4)))
assn0ToI = Assignment i (ALit 0)
assn0ToR = Assignment r (ALit 0)
iLessN = BComp Less (mkIdt i) (mkIdt n)
inv = BForall (Idt "k") (BBinExp Implies kInRange aAtKLessOrEqR)
kInRange = BPredicate (Idt "inRange") [mkIdt k, ALit 0, mkIdt i]
aAtKLessOrEqR = BComp LessOrEqual (mkIdt (LArray a (mkIdt k))) (mkIdt r)
aAtRLessAAtI = BComp Less (mkIdt (LArray a (mkIdt r))) (mkIdt (LArray a (mkIdt i)))
assnIToR = Assignment r (mkIdt i)
assnIPlus1ToI = Assignment i (ABinExp Add (mkIdt i) (ALit 1))
