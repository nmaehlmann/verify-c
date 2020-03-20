module IT.FOExpSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.BooleanExpression

    
fOExpSpec :: IO Spec
fOExpSpec = return $ describe "Parser.BooleanExpression" $ do
    let parseFOExp t = parse bExpFO "" t
    let x = AIdt $ LIdt $ Idt "x"
    let y = AIdt $ LIdt $ Idt "y"
    let n = AIdt $ LIdt $ Idt "n"
    let xLess1 = BComp Less x (ALit 1)

    it "parses boolean constants" $ do
        parseFOExp "true" `shouldBe` (Right BTrue)
        parseFOExp "false" `shouldBe` (Right BFalse)

    it "parses comparisons" $ do
        parseFOExp "x == 1" `shouldBe` (Right (BComp Equal x (ALit 1)))
        parseFOExp "x != 1" `shouldBe` (Right (BComp NotEqual x (ALit 1)))
        parseFOExp "x >= 1" `shouldBe` (Right (BComp GreaterOrEqual x (ALit 1)))
        parseFOExp "x <= 1" `shouldBe` (Right (BComp LessOrEqual x (ALit 1)))
        parseFOExp "x > 1" `shouldBe` (Right (BComp Greater x (ALit 1)))
        parseFOExp "x < 1" `shouldBe` (Right xLess1)

    it "parses negations" $ do
        parseFOExp "!(x < 1)" `shouldBe` (Right (BNeg xLess1))

    it "parses unary predicates" $ do
        parseFOExp "isPrime(x)" `shouldBe` (Right (BPredicate (Idt "isPrime") [x]))

    it "parses forall quantifiers" $ do
        parseFOExp "forall(x, x < 1)" `shouldBe` (Right (BForall (Idt "x") xLess1))

    it "parses exists quantifiers" $ do
        parseFOExp "exists(x, x < 1)" `shouldBe` (Right (BExists (Idt "x") xLess1))

    it "parses binary predicates" $ do    
        parseFOExp "isSuccessorOf(x,y)" `shouldBe` (Right (BPredicate (Idt "isSuccessorOf") [x,y]))

    it "parses functions" $ do
        let fibN = AFunCall (Idt "fib") [n]
        parseFOExp "x == fib(n)" `shouldBe` (Right (BComp Equal x fibN))
        parseFOExp "x == fib(n) + fib(n)" `shouldBe` (Right (BComp Equal x (ABinExp Add fibN fibN)))

    it "parses binary expressions" $ do
        parseFOExp "(x < 1) && (x < 1)" `shouldBe` (Right (BBinExp And xLess1 xLess1))
        parseFOExp "x < 1 && x < 1" `shouldBe` (Right (BBinExp And xLess1 xLess1))
        parseFOExp "x < 1 || x < 1" `shouldBe` (Right (BBinExp Or xLess1 xLess1))
        parseFOExp "x < 1 -> x < 1" `shouldBe` (Right (BBinExp Implies xLess1 xLess1))
