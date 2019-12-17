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
    let parseStmt t = parse statement "" t
    let x = Idt "x"
    let y = Idt "y"
    let z = Idt "z"
    let a = Idt "a"
    let b = Idt "b"

    it "parses assignments" $ do
        parseStmt "x = y;" `shouldBe` (Right (Assignment x (AIdt y)))

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