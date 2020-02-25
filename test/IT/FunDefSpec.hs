module IT.FunDefSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.FunctionDefinition

funDefSpec :: IO Spec
funDefSpec = return $ describe "Parser.FunctionDefinition" $ do
    let parseFunDef t = parse funDef "" t
    let x = LIdt $ Idt "x"
    let a = LIdt $ Idt "a"
    let b = LIdt $ Idt "b"

    it "parses void functions" $ do
        let s = "void noOp(){ precondition(\"true\"); postcondition(\"true\"); return; }"
        let result = FunctionDefinition
                { funDefType     = TVoid
                , funDefName     = Idt "noOp"
                , funDefArgs     = []
                , funDefPrecond  = FOTrue
                , funDefPostcond = FOTrue
                , funDefBody     = Return Nothing
                }
        parseFunDef s `shouldBe` (Right result)

    it "parses functions without arguments" $ do
        let s = "int one(){ precondition(\"true\"); postcondition(\"true\"); return 1; }"
        let result = FunctionDefinition
                { funDefType     = TInt
                , funDefName     = Idt "one"
                , funDefArgs     = []
                , funDefPrecond  = FOTrue
                , funDefPostcond = FOTrue
                , funDefBody     = Return $ Just $ ALit $ 1
                }
        parseFunDef s `shouldBe` (Right result)
        

    it "parses a simple add function" $ do
        let s = "int add(int a, int b){ precondition(\"true\"); postcondition(\"x == a + b\"); return a + b; }"
        let result = FunctionDefinition
                { funDefType     = TInt
                , funDefName     = Idt "add"
                , funDefArgs     = [Decl TInt (Idt "a"), Decl TInt (Idt "b")]
                , funDefPrecond  = FOTrue
                , funDefPostcond = FOComp Equal (AIdt x) $ ABinExp Add (AIdt a) (AIdt b)
                , funDefBody     = Return $ Just $ ABinExp Add (AIdt a) (AIdt b)
                }
        parseFunDef s `shouldBe` (Right result)

