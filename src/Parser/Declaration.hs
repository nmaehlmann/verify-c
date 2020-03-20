module Parser.Declaration (declaration) where

import Text.Parsec.String (Parser)

import AST
import Parser.Identifier
import Parser.Type

declaration :: Parser Decl
declaration = Decl <$> typeName <*> identifier
    -- typ <- typeName
    -- name <- identifier
    -- return $ Decl typ name
