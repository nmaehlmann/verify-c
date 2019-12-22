module Parser.Type (typeName) where

import Text.Parsec
import Text.Parsec.String (Parser)

import AST
import Parser.Lexer

typeName :: Parser Type
typeName = tVoid <|> tInt <|> tChar

tInt :: Parser Type
tInt = reserved "int" >> return TInt

tChar :: Parser Type
tChar = reserved "char" >> return TChar

tVoid :: Parser Type
tVoid = reserved "void" >> return TVoid