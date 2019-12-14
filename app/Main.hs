module Main where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Lexer
import Lib
import ArithmeticExpression
import BooleanExpression
import Identifier

main :: IO ()
main = someFunc

assignment :: Parser Stmt
assignment = do
    id <- identifier
    reservedOp "="
    exp <- aExp
    return $ Assignment id exp

