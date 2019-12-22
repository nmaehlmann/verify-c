module Parser.Declaration (declaration) where

import Text.Parsec.String (Parser)

import AST
import Parser.Lexer
import Parser.Identifier
import Parser.Type

declaration :: Parser Decl
declaration = do
    typ <- typeName
    name <- identifier
    return $ Decl typ name
