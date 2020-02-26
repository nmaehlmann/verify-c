module IT.StatementSpec where

import Test.Hspec
import Text.Parsec
import Data.Either

import AST
import Parser.Statement

import IT.Whitespaces

statementSpec :: IO Spec
statementSpec = return $ describe "Parser.Statement" $ do
    let isValid = isRight
    let parseStmt t = parse statement "" t
    let mkIdt = AIdt . ReadLExp
    let x = LIdt $ Idt "x"
    let y = LIdt $ Idt "y"
    let z = LIdt $ Idt "z"
    let a = LIdt $ Idt "a"
    let b = LIdt $ Idt "b"

    it "rejects invalid statements" $ do
        parseStmt "this is not a valid c0 program" `shouldNotSatisfy` isValid

    it "parses assignments" $ do
        parseStmt "x = y;" `shouldBe` (Right (Assignment x (mkIdt y)))

    it "parses assignments to fields of an array" $ do
        parseStmt "a[1] = y;" `shouldBe` (Right (Assignment (LArray a (ALit 1)) (mkIdt y)))
    
    it "parses assignments to arrays" $ do
        let arrayExp = AArray $ ALit <$> [1..6]
        parseStmt "a = {1, 2, 3, 4, 5, 6};" `shouldBe` (Right (Assignment a arrayExp))

    it "ignores whitespaces in assignments" $ do
        whitespaceIndependent ["x", "=", "y", ";"] $ 
            \s -> parseStmt s  `shouldBe` (Right (Assignment x (mkIdt y)))

    it "parses sequences of assignments" $ do
        let xAssn = Assignment x (mkIdt y)
        let yAssn = Assignment y (mkIdt z)
        let aAssn = Assignment a (mkIdt b)
        parseStmt "x = y; y = z; a = b;"  `shouldBe` (Right (Seq (Seq xAssn yAssn) aAssn))

    it "parses if-then-else blocks" $ do
        let s = "if( a < b ){ x = y; } else{ y = z; }"
        let condition = BComp Less (mkIdt a) (mkIdt b)
        let ifCase = Assignment x (mkIdt y)
        let elseCase = Assignment y (mkIdt z)
        parseStmt s `shouldBe` (Right (ITE condition ifCase elseCase))

    it "parses while blocks" $ do
        let s = "while( 0 < x ){ invariant(\"true\"); x = x - 1; }"
        let condition = BComp Less (ALit 0) (mkIdt x)
        let inv = FOTrue
        let body = Assignment x (ABinExp Sub (mkIdt x) (ALit 1))
        parseStmt s `shouldBe` (Right (While condition inv body))

    it "parses assertions" $ do
        let s = "assertion(\"x != 0\");"
        parseStmt s `shouldBe` (Right (Assertion (FOComp NotEqual (mkIdt x) (ALit 0))))

