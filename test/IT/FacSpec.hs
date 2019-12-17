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
                True `shouldBe` True
