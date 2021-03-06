module Parser.ArithmeticExpressionSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.ArithmeticExpression
import Parser.Whitespaces

spec :: Spec
spec = describe "aExpFO" $ do
    let parseAExp t = parse aExpFO "" t
    let x = AIdt $ LIdt $ Idt "x"
    let y = AIdt $ LIdt $ Idt "y"
    let z = AIdt $ LIdt $ Idt "z"

    it "parses literals" $ do
        parseAExp "1337" `shouldBe` (Right (ALit 1337))

    it "parses negative literals" $ do
        parseAExp "-1337" `shouldBe` (Right (ALit (-1337)))

    it "parses arithmetic operators" $ do
        parseAExp "1 + 2" `shouldBe` (Right (ABinExp Add (ALit 1) (ALit 2)))
        parseAExp "x - y" `shouldBe` (Right (ABinExp Sub x y))
        parseAExp "x * y" `shouldBe` (Right (ABinExp Mul x y))
        parseAExp "x / y" `shouldBe` (Right (ABinExp Div x y))

    it "parses arrays" $ do
        parseAExp "x + y[z]" `shouldBe` (Right (ABinExp Add x (AIdt (LArray (LIdt (Idt "y")) z))))

    it "ignores whitespaces" $ do
        whitespaceIndependent ["x", "+", "y"] $ 
            \s -> parseAExp s  `shouldBe` (Right (ABinExp Add x y))

    it "parses addition left associative" $ do
        parseAExp "x + y + z" `shouldBe` (Right (ABinExp Add (ABinExp Add x y) z))

    it "parses multiplication left associative" $ do
        parseAExp "x * y * z" `shouldBe` (Right (ABinExp Mul (ABinExp Mul x y) z))

    it "parses multiplication with higher precedence than addition" $ do
        parseAExp "x + y * z" `shouldBe` (Right (ABinExp Add  x (ABinExp Mul y z)))

    it "parses nested parentheses" $ do
        parseAExp "x - (3 / (y + z))" `shouldBe` (Right (ABinExp Sub  x (ABinExp Div (ALit 3) (ABinExp Add y z))))

    it "parses parentheses with higher precedence than multiplication" $ do
        parseAExp "x * (y - z)" `shouldBe` (Right (ABinExp Mul x (ABinExp Sub y z)))

    it "parses function calls" $ do
        parseAExp "pow(10, 2) + 1" `shouldBe` (Right (ABinExp Add (AFunCall (Idt "pow") [(ALit 10), (ALit 2)]) (ALit 1)))

    it "distinguishes function calls from LExpressions" $ do
        parseAExp "x + pow(10, 2)" `shouldBe` (Right (ABinExp Add x (AFunCall (Idt "pow") [(ALit 10), (ALit 2)])))
