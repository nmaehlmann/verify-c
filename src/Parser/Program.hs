module Parser.Program where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (Empty)

import AST
import Parser.FunctionDefinition
import Parser.Lexer

program :: Parser Program
program = whiteSpace >> Program <$> many1 funDef