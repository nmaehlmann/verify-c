module Main where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Lexer
import Lib

main :: IO ()
main = someFunc

identifier :: Parser Idt
identifier = Idt <$> lIdentifier

assignment :: Parser Stmt
assignment = do
    id <- identifier
    reservedOp "="
    exp <- aExp
    return $ Assignment id exp

aExp :: Parser AExp
aExp = buildExpressionParser aOperatorTable aTerm

aTerm :: Parser AExp
aTerm = parens aExp <|> aLit <|> aIdt

aOperatorTable :: OperatorTable String () Identity AExp
aOperatorTable =
    [ [Infix opMul AssocLeft, Infix opDiv AssocLeft]
    , [Infix opAdd AssocLeft, Infix opSub AssocLeft]
    ]

opMul, opDiv, opAdd, opSub :: Parser (AExp -> AExp -> AExp)
opMul = reservedOp "*" >> return (ABinExp Mul)
opDiv = reservedOp "/" >> return (ABinExp Div)
opAdd = reservedOp "+" >> return (ABinExp Add)
opSub = reservedOp "-" >> return (ABinExp Sub)

aLit :: Parser AExp
aLit = ALit <$> integer

aIdt :: Parser AExp
aIdt = AIdt <$> identifier