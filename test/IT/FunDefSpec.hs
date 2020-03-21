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
                , funDefPrecond  = BTrue
                , funDefPostcond = BTrue
                , funDefBody     = Return Nothing
                }
        parseFunDef s `shouldBe` (Right result)

    it "parses boolean expressions in preconditions" $ do
        let s = "void noOp(){ precondition(\"a != *b\"); postcondition(\"true\"); return; }"
        let result = FunctionDefinition
                { funDefType     = TVoid
                , funDefName     = Idt "noOp"
                , funDefArgs     = []
                , funDefPrecond  = BComp NotEqual (AIdt a) (AIdt (LDeref b))
                , funDefPostcond = BTrue
                , funDefBody     = Return Nothing
                }
        parseFunDef s `shouldBe` (Right result)

    it "parses functions without arguments" $ do
        let s = "int one(){ precondition(\"true\"); postcondition(\"true\"); return 1; }"
        let result = FunctionDefinition
                { funDefType     = TInt
                , funDefName     = Idt "one"
                , funDefArgs     = []
                , funDefPrecond  = BTrue
                , funDefPostcond = BTrue
                , funDefBody     = Return $ Just $ ALit $ 1
                }
        parseFunDef s `shouldBe` (Right result)
        

    it "parses a simple add function" $ do
        let mkIdt = AIdt
        let s = "int add(int a, int b){ precondition(\"true\"); postcondition(\"x == a + b\"); return a + b; }"
        let result = FunctionDefinition
                { funDefType     = TInt
                , funDefName     = Idt "add"
                , funDefArgs     = [Decl TInt (Idt "a"), Decl TInt (Idt "b")]
                , funDefPrecond  = BTrue
                , funDefPostcond = BComp Equal (mkIdt x) $ ABinExp Add (mkIdt a) (mkIdt b)
                , funDefBody     = Return $ Just $ ABinExp Add (mkIdt a) (mkIdt b)
                }
        parseFunDef s `shouldBe` (Right result)

    it "parses a function with 1 reference argument" $ do
        let s = "void noSwap(int* a){ precondition(\"true\"); postcondition(\"true\"); return; }"
        let result = FunctionDefinition
                { funDefType     = TVoid
                , funDefName     = Idt "noSwap"
                , funDefArgs     = [Decl (TReference TInt) (Idt "a")]
                , funDefPrecond  = BTrue
                , funDefPostcond = BTrue
                , funDefBody     = Return Nothing
                }
        parseFunDef s `shouldBe` (Right result)

    it "parses a function with 2 reference arguments" $ do
        let s = "void noSwap(int* a, int* b){ precondition(\"true\"); postcondition(\"true\"); return; }"
        let result = FunctionDefinition
                { funDefType     = TVoid
                , funDefName     = Idt "noSwap"
                , funDefArgs     = [Decl (TReference TInt) (Idt "a"), Decl (TReference TInt) (Idt "b")]
                , funDefPrecond  = BTrue
                , funDefPostcond = BTrue
                , funDefBody     = Return Nothing
                }
        parseFunDef s `shouldBe` (Right result)


