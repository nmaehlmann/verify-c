module IT.BExpSpec where

import Test.Hspec
import Text.Parsec
import Data.Either

import AST
import Parser.BooleanExpression

import IT.Whitespaces
    
spec :: Spec
spec = describe "Parser.BooleanExpression" $ do
    let parseBExp t = parse bExpC0 "" t
    let x = AIdt $ LIdt $ Idt "x"
    let y = AIdt $ LIdt $ Idt "y"
    let z = AIdt $ LIdt $ Idt "z"
    let isValid = isRight

    it "parses boolean constants" $ do
        parseBExp "true" `shouldBe` Right BTrue
        parseBExp "false" `shouldBe` Right BFalse
        parseBExp "maybe" `shouldNotSatisfy` isValid

    it "parses simple comparisons" $ do
        parseBExp "x == y" `shouldBe` (Right (BComp Equal x y))
        parseBExp "x != y" `shouldBe` (Right (BComp NotEqual x y))
        parseBExp "x <  y" `shouldBe` (Right (BComp Less x y))
        parseBExp "x <= y" `shouldBe` (Right (BComp LessOrEqual x y))
        parseBExp "x >  y" `shouldBe` (Right (BComp Greater x y))
        parseBExp "x >= y" `shouldBe` (Right (BComp GreaterOrEqual x y))

    it "ignores whitespaces in '<' comparisons" $ do
        whitespaceIndependent ["x","<","y"] $ \s -> parseBExp s `shouldBe` (Right (BComp Less x y))
    
    it "ignores whitespaces in '=' comparisons" $ do
        whitespaceIndependent ["x","==","y"] $ \s -> parseBExp s `shouldBe` (Right (BComp Equal x y))

    it "parses simple conjunctions" $ do
        parseBExp "x < y && y < z" `shouldBe` (Right (BBinExp And (BComp Less x y) (BComp Less y z)))

    it "parses simple disjunctions" $ do
        parseBExp "x < y || y < z" `shouldBe` (Right (BBinExp Or (BComp Less x y) (BComp Less y z)))

    it "ignores whitespaces in conjunctions" $ do
        whitespaceIndependent ["x < y", "&&", "y < z"] $ 
            \s -> parseBExp s `shouldBe` (Right (BBinExp And (BComp Less x y) (BComp Less y z)))

    it "ignores whitespaces in disjunctions" $ do
        whitespaceIndependent ["x < y", "||", "y < z"] $ 
            \s -> parseBExp s `shouldBe` (Right (BBinExp Or (BComp Less x y) (BComp Less y z)))

    it "parses parentheses" $ do
        parseBExp "(x < y) || ((y < z) && true)" `shouldBe` (Right (BBinExp Or (BComp Less x y) (BBinExp And (BComp Less y z) BTrue)))
        
                