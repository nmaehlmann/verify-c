module Identifier where

import Text.Parsec.String (Parser)
import Text.Parsec

import AST
import Lexer

identifier :: Parser Idt
identifier = Idt <$> lIdentifier