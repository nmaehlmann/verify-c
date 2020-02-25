module IT.FOExpSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.BooleanExpression

    
fOExpSpec :: IO Spec
fOExpSpec = return $ describe "Parser.BooleanExpression" $ do
    let parseFOExp t = parse fOExp "" t
    let x = AIdt $ LIdt $ Idt "x"
    let y = AIdt $ LIdt $ Idt "y"
    let n = AIdt $ LIdt $ Idt "n"
    let xLess1 = FOComp Less x (ALit 1)

    it "parses boolean constants" $ do
        parseFOExp "true" `shouldBe` (Right FOTrue)
        parseFOExp "false" `shouldBe` (Right FOFalse)

    it "parses comparisons" $ do
        parseFOExp "x == 1" `shouldBe` (Right (FOComp Equal x (ALit 1)))
        parseFOExp "x != 1" `shouldBe` (Right (FOComp NotEqual x (ALit 1)))
        parseFOExp "x >= 1" `shouldBe` (Right (FOComp GreaterOrEqual x (ALit 1)))
        parseFOExp "x <= 1" `shouldBe` (Right (FOComp LessOrEqual x (ALit 1)))
        parseFOExp "x > 1" `shouldBe` (Right (FOComp Greater x (ALit 1)))
        parseFOExp "x < 1" `shouldBe` (Right xLess1)

    it "parses negations" $ do
        parseFOExp "!(x < 1)" `shouldBe` (Right (FONeg xLess1))

    it "parses unary predicates" $ do
        parseFOExp "isPrime(x)" `shouldBe` (Right (Predicate (Idt "isPrime") [x]))

    it "parses forall quantifiers" $ do
        parseFOExp "forall(x, x < 1)" `shouldBe` (Right (Forall (Idt "x") xLess1))

    it "parses exists quantifiers" $ do
        parseFOExp "exists(x, x < 1)" `shouldBe` (Right (Exists (Idt "x") xLess1))

    it "parses binary predicates" $ do    
        parseFOExp "isSuccessorOf(x,y)" `shouldBe` (Right (Predicate (Idt "isSuccessorOf") [x,y]))

    it "parses functions" $ do
        let fibN = AFunCall (Idt "fib") [n]
        parseFOExp "x == fib(n)" `shouldBe` (Right (FOComp Equal x fibN))
        parseFOExp "x == fib(n) + fib(n)" `shouldBe` (Right (FOComp Equal x (ABinExp Add fibN fibN)))

    it "parses binary expressions" $ do
        parseFOExp "(x < 1) && (x < 1)" `shouldBe` (Right (FOBinExp And xLess1 xLess1))
        parseFOExp "x < 1 && x < 1" `shouldBe` (Right (FOBinExp And xLess1 xLess1))
        parseFOExp "x < 1 || x < 1" `shouldBe` (Right (FOBinExp Or xLess1 xLess1))
        parseFOExp "x < 1 -> x < 1" `shouldBe` (Right (FOBinExp Implies xLess1 xLess1))
