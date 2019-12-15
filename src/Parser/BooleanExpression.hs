module Parser.BooleanExpression where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.ArithmeticExpression
import Parser.Lexer
import Parser.Identifier
    
bExp :: Parser BExp
bExp = buildExpressionParser bOperatorTable bTerm

bTerm :: Parser BExp
bTerm = parens bExp <|> bTrue <|> bFalse <|> try bEq <|> bLess

bEq, bLess :: Parser BExp
bEq = comparisonBTerm "=" BEq
bLess = comparisonBTerm "<" BLess

comparisonBTerm :: String -> (AExp -> AExp -> BExp) -> Parser BExp
comparisonBTerm op f = do
    lhs <- aExp
    reservedOp op
    rhs <- aExp
    return $ f lhs rhs

bOperatorTable :: OperatorTable String () Identity BExp
bOperatorTable =
    [ [Prefix opNeg]
    , [Infix opAnd AssocLeft, Infix opOr AssocLeft]
    ]

opAnd, opOr :: Parser (BExp -> BExp -> BExp)
opAnd = reservedOp "&&" >> return BAnd
opOr = reservedOp "||" >> return BOr

opNeg :: Parser (BExp -> BExp)
opNeg = reservedOp "!" >> return BNeg


bTrue, bFalse :: Parser BExp
bTrue = reserved "true" >> return  BTrue
bFalse = reserved "false" >> return  BFalse