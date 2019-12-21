module IT.BExpSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Data.Either

import AST
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.Statement

import IT.Whitespaces
    
bExpSpec :: IO Spec
bExpSpec = return $ describe "Parser.BooleanExpression" $ do
    let parseBExp t = parse bExp "" t
    let x = AIdt $ LIdt "x"
    let y = AIdt $ LIdt "y"
    let z = AIdt $ LIdt "z"
    let isValid = isRight

    it "parses boolean constants" $ do
        parseBExp "true" `shouldBe` Right BTrue
        parseBExp "false" `shouldBe` Right BFalse
        parseBExp "maybe" `shouldNotSatisfy` isValid

    it "parses simple comparisons" $ do
        parseBExp "x < y" `shouldBe` (Right (BLess x y))
        parseBExp "x == y" `shouldBe` (Right (BEq x y))

    it "ignores whitespaces in '<' comparisons" $ do
        whitespaceIndependent ["x","<","y"] $ \s -> parseBExp s `shouldBe` (Right (BLess x y))
    
    it "ignores whitespaces in '=' comparisons" $ do
        whitespaceIndependent ["x","==","y"] $ \s -> parseBExp s `shouldBe` (Right (BEq x y))

    it "parses simple conjunctions" $ do
        parseBExp "x < y && y < z" `shouldBe` (Right (BAnd (BLess x y) (BLess y z)))

    it "parses simple disjunctions" $ do
        parseBExp "x < y || y < z" `shouldBe` (Right (BOr (BLess x y) (BLess y z)))

    it "ignores whitespaces in conjunctions" $ do
        whitespaceIndependent ["x < y", "&&", "y < z"] $ 
            \s -> parseBExp s `shouldBe` (Right (BAnd (BLess x y) (BLess y z)))

    it "ignores whitespaces in disjunctions" $ do
        whitespaceIndependent ["x < y", "||", "y < z"] $ 
            \s -> parseBExp s `shouldBe` (Right (BOr (BLess x y) (BLess y z)))

    it "parses parentheses" $ do
        parseBExp "(x < y) || ((y < z) && true)" `shouldBe` (Right (BOr (BLess x y) (BAnd (BLess y z) BTrue)))