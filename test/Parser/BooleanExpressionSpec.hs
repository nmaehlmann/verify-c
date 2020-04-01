module Parser.BooleanExpressionSpec where

import Test.Hspec
import Text.Parsec
import Data.Either

import AST
import Parser.BooleanExpression

import Parser.Whitespaces
    
spec :: Spec
spec = do
    describe "bExpC0" $ do
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

    describe "bExpFO" $ do
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
            parseFOExp "x < 1 && x < 1" `shouldBe` (Right (BBinExp And xLess1 xLess1))
            parseFOExp "x < 1 || x < 1" `shouldBe` (Right (BBinExp Or xLess1 xLess1))
            parseFOExp "x < 1 -> x < 1" `shouldBe` (Right (BBinExp Implies xLess1 xLess1))

        it "parses parens" $ do
            parseFOExp "(((x < 1)))" `shouldBe` (Right xLess1)
            parseFOExp "(x < 1) && (x < 1)" `shouldBe` (Right (BBinExp And xLess1 xLess1))
            parseFOExp "((x < 1) && (x < 1))" `shouldBe` (Right (BBinExp And xLess1 xLess1))

        it "parses the postcondition of a max program" $ do
            let idx = AIdt $ LIdt $ Idt "idx"
            let len = AIdt $ LIdt $ Idt "len"
            let result = AIdt $ LIdt $ Idt "\\result"
            let arr = LIdt $ Idt "arr"
            let arrAtIdx = AIdt $ LArray arr idx
            
            let input = "forall(idx, (idx >= 0 && idx < len) -> \\result >= arr[idx])"
            let expectedOutput = BForall (Idt "idx") $ 
                    BBinExp Implies 
                        (BBinExp And 
                            (BComp GreaterOrEqual idx (ALit 0))
                            (BComp Less idx len))
                        (BComp GreaterOrEqual result arrAtIdx)

            parseFOExp input `shouldBe` (Right expectedOutput)