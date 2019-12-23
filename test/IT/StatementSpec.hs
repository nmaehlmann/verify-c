module IT.StatementSpec where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Data.Either

import AST
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.Statement

import IT.Whitespaces

statementSpec :: IO Spec
statementSpec = return $ describe "Parser.Statement" $ do
    let isValid = isRight
    let parseStmt t = parse statement "" t
    let x = LIdt $ Idt "x"
    let y = LIdt $ Idt "y"
    let z = LIdt $ Idt "z"
    let a = LIdt $ Idt "a"
    let b = LIdt $ Idt "b"

    it "rejects invalid statements" $ do
        parseStmt "this is not a valid c0 program" `shouldNotSatisfy` isValid

    it "parses assignments" $ do
        parseStmt "x = y;" `shouldBe` (Right (Assignment x (AIdt y)))

    it "parses assignments to fields of an array" $ do
        parseStmt "a[1] = y;" `shouldBe` (Right (Assignment (LArray a (ALit 1)) (AIdt y)))
    
    it "parses assignments to arrays" $ do
        let arrayExp = AArray $ ALit <$> [1..6]
        parseStmt "a = {1, 2, 3, 4, 5, 6};" `shouldBe` (Right (Assignment a arrayExp))

    it "ignores whitespaces in assignments" $ do
        whitespaceIndependent ["x", "=", "y", ";"] $ 
            \s -> parseStmt s  `shouldBe` (Right (Assignment x (AIdt y)))

    it "parses sequences of assignments" $ do
        let xAssn = Assignment x (AIdt y)
        let yAssn = Assignment y (AIdt z)
        let aAssn = Assignment a (AIdt b)
        parseStmt "x = y; y = z; a = b;"  `shouldBe` (Right (Seq (Seq xAssn yAssn) aAssn))

    it "parses if-then-else blocks" $ do
        let s = "if( a < b ){ x = y; } else{ y = z; }"
        let condition = BLess (AIdt a) (AIdt b)
        let ifCase = Assignment x (AIdt y)
        let elseCase = Assignment y (AIdt z)
        parseStmt s `shouldBe` (Right (ITE condition ifCase elseCase))

    it "parses while blocks" $ do
        let s = "while( 0 < x ){ x = x - 1; }"
        let condition = BLess (ALit 0) (AIdt x)
        let body = Assignment x (ABinExp Sub (AIdt x) (ALit 1))
        parseStmt s `shouldBe` (Right (While condition body))

    it "parses void functions" $ do
        let s = "void noOp(){return;}"
        let returnType = TVoid
        let name = Idt "noOp"
        let arguments = []
        let body = Return Nothing
        parseStmt s `shouldBe` (Right (FunDef returnType name arguments body))

    it "parses functions without arguments" $ do
        let s = "int one(){return 1;}"
        let returnType = TInt
        let name = Idt "one"
        let arguments = []
        let body = Return $ Just $ ALit $ 1
        parseStmt s `shouldBe` (Right (FunDef returnType name arguments body))

    it "parses simple functions definitions" $ do
        let s = "int add(int a, int b){return a + b;}"
        let returnType = TInt
        let name = Idt "add"
        let arguments = [Decl TInt (Idt "a"), Decl TInt (Idt "b")]
        let body = Return $ Just $ ABinExp Add (AIdt a) (AIdt b)
        parseStmt s `shouldBe` (Right (FunDef returnType name arguments body))

