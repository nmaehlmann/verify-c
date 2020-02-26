module IT.LExpSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.LExpression

lExpSpec :: IO Spec
lExpSpec = return $ describe "Parser.LExpression" $ do
    let parseLExp t = parse lExp "" t
    let a = LIdt $ Idt "a"
    let arr = LIdt $ Idt "arr"
    let x = AIdt $ ReadLExp $ LIdt $ Idt "x"
    let y = AIdt $ ReadLExp $ LIdt $ Idt "y"
    
    it "parses symbols" $ do
        parseLExp "pi" `shouldBe` (Right (LIdt (Idt "pi")))

    it "parses arrays" $ do
        parseLExp "a[1]" `shouldBe` (Right (LArray a (ALit 1)))
    
    it "parses two-dimensional arrays" $ do
        parseLExp "a[1][99]" `shouldBe` (Right (LArray (LArray a (ALit 1)) (ALit 99)))

    it "parses three-dimensional arrays" $ do
        parseLExp "a[1][99][12]" `shouldBe` 
            (Right (LArray ((LArray (LArray a (ALit 1)) (ALit 99))) (ALit 12)))

    it "parses arrays with calculated indices" $ do
        parseLExp "arr[x * y]" `shouldBe` (Right (LArray arr (ABinExp Mul x y)))

    it "parses derefenced symbols" $ do
        parseLExp "*referencedVar" `shouldBe` (Right (LDereference (LIdt (Idt "referencedVar"))))

    it "parses doubly derefenced symbols" $ do
        parseLExp "**a" `shouldBe` (Right (LDereference (LDereference a)))

    it "parses triply derefenced symbols" $ do
        parseLExp "***a" `shouldBe` (Right (LDereference (LDereference (LDereference a))))    

    it "parses array brackets with higher precedence than derefs" $ do
        parseLExp "*a[x]" `shouldBe` (Right (LDereference (LArray a x)))

    it "parses structure parts" $ do
        parseLExp "arr.a" `shouldBe` (Right (LStructPart arr a))

    it "parses structure parts" $ do
        parseLExp "arr.a.arr" `shouldBe` (Right (LStructPart (LStructPart arr a) arr))