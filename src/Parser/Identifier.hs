module Parser.Identifier where

import Text.Parsec.String (Parser)
import Text.Parsec

import AST
import Parser.Lexer

identifier :: Parser Idt
identifier = Idt <$> lIdentifier