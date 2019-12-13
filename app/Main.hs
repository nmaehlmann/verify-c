module Main where

import Lib
import AST
import Lexer

import Text.Parsec.String (Parser)
import Text.Parsec

main :: IO ()
main = someFunc

assignment :: Parser Stmt
assignment = do
    id <- Idt <$> identifier
    reservedOp "="
    ref <- Idt <$> identifier
    return $ Assignment id $ AIdt ref