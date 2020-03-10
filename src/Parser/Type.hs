module Parser.Type (typeName) where

import Text.Parsec
import Text.Parsec.String (Parser)

import AST
import Parser.Lexer

typeName :: Parser Type
typeName = do
    baseType <- tBaseType
    refs <- many opReference
    return $ foldl (\t ref -> ref t) baseType refs

tBaseType :: Parser Type
tBaseType = tVoid <|> tInt <|> tChar

opReference :: Parser (Type -> Type)
opReference = reserved "*" >> return TReference

tInt :: Parser Type
tInt = reserved "int" >> return TInt

tChar :: Parser Type
tChar = reserved "char" >> return TChar

tVoid :: Parser Type
tVoid = reserved "void" >> return TVoid