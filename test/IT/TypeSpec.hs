module IT.TypeSpec where

import Test.Hspec
import Text.Parsec
import Data.Either

import AST
import Parser.Type

import IT.Whitespaces

typeSpec :: IO Spec
typeSpec = return $ describe "Parser.Statement" $ do
    let parseStmt t = parse typeName "" t

    it "parses void" $ do
        parseStmt "void" `shouldBe` (Right TVoid)

    it "parses int" $ do
        parseStmt "int" `shouldBe` (Right TInt)

    it "parses char" $ do
        parseStmt "char" `shouldBe` (Right TChar)

    it "parses int references" $ do
        parseStmt "int*" `shouldBe` (Right (TReference TInt))