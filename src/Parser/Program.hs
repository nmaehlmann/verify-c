module Parser.Program where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (Empty)

import AST
import Parser.Declaration
import Parser.Identifier
import Parser.Type
import Parser.Statement
import Parser.Lexer
import Parser.FunctionDefinition

program :: Parser Program
program = fmap Program $ many1 funDef