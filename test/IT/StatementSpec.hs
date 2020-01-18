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
        let condition = BComp Less (AIdt a) (AIdt b)
        let ifCase = Assignment x (AIdt y)
        let elseCase = Assignment y (AIdt z)
        parseStmt s `shouldBe` (Right (ITE condition ifCase elseCase))

    it "parses while blocks" $ do
        let s = "while( 0 < x ){ invariant(\"true\"); x = x - 1; }"
        let condition = BComp Less (ALit 0) (AIdt x)
        let inv = FOTrue
        let body = Assignment x (ABinExp Sub (AIdt x) (ALit 1))
        parseStmt s `shouldBe` (Right (While condition inv body))

    it "parses void functions" $ do
        let s = "void noOp(){ precondition(\"true\"); postcondition(\"true\"); return; }"
        let result = FunDef $ FunctionDefinition
                { funDefType     = TVoid
                , funDefName     = Idt "noOp"
                , funDefArgs     = []
                , funDefPrecond  = FOTrue
                , funDefPostcond = FOTrue
                , funDefBody     = Return Nothing
                }
        parseStmt s `shouldBe` (Right result)

    it "parses functions without arguments" $ do
        let s = "int one(){ precondition(\"true\"); postcondition(\"true\"); return 1; }"
        let result = FunDef $ FunctionDefinition
                { funDefType     = TInt
                , funDefName     = Idt "one"
                , funDefArgs     = []
                , funDefPrecond  = FOTrue
                , funDefPostcond = FOTrue
                , funDefBody     = Return $ Just $ ALit $ 1
                }
        parseStmt s `shouldBe` (Right result)
        

    it "parses a simple add function" $ do
        let s = "int add(int a, int b){ precondition(\"true\"); postcondition(\"x == a + b\"); return a + b; }"
        let result = FunDef $ FunctionDefinition
                { funDefType     = TInt
                , funDefName     = Idt "add"
                , funDefArgs     = [Decl TInt (Idt "a"), Decl TInt (Idt "b")]
                , funDefPrecond  = FOTrue
                , funDefPostcond = FOComp Equal (AIdt x) $ ABinExp Add (AIdt a) (AIdt b)
                , funDefBody     = Return $ Just $ ABinExp Add (AIdt a) (AIdt b)
                }
        parseStmt s `shouldBe` (Right result)

    it "parses assertions" $ do
        let s = "assertion(\"x != 0\");"
        parseStmt s `shouldBe` (Right (Assertion (FOComp NotEqual (AIdt x) (ALit 0))))

