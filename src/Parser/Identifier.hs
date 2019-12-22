module Parser.Identifier (identifier) where

import Text.Parsec.String (Parser)

import AST
import Parser.Lexer

identifier :: Parser Idt
identifier = Idt <$> lIdentifier