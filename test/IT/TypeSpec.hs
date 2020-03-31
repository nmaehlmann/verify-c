module IT.TypeSpec where

import Test.Hspec
import Text.Parsec

import AST
import Parser.Type

spec :: Spec
spec = describe "Parser.Statement" $ do
    let parseStmt t = parse typeName "" t

    it "parses void" $ do
        parseStmt "void" `shouldBe` (Right TVoid)

    it "parses int" $ do
        parseStmt "int" `shouldBe` (Right TInt)

    it "parses int references" $ do
        parseStmt "int*" `shouldBe` (Right (TReference TInt))