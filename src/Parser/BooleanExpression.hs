module Parser.BooleanExpression where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.ArithmeticExpression
import Parser.Lexer
    
bExp :: Parser BExp
bExp = buildExpressionParser bOperatorTable bTerm

bTerm :: Parser BExp
bTerm = parens bExp <|> bTrue <|> bFalse <|> comparison

comparison :: Parser BExp
comparison = do
    lhs <- aExp
    op  <- comparisonOperator
    rhs <- aExp
    return $ BComp op lhs rhs

comparisonOperator :: Parser CompOp
comparisonOperator = choice $ toParser <$> comparisonOperators
    where toParser (txt, op) = try $ reservedOp txt >> return op

comparisonOperators :: [(String, CompOp)]
comparisonOperators = 
    [ ("==", Equal)
    , ("!=", NotEqual)
    , ("<=", LessOrEqual)
    , ("<" , Less)
    , (">=", GreaterOrEqual)
    , (">" , Greater)
    ]

bOperatorTable :: OperatorTable String () Identity BExp
bOperatorTable =
    [ [Prefix opNeg]
    , [Infix opAnd AssocLeft, Infix opOr AssocLeft]
    ]

opAnd, opOr :: Parser (BExp -> BExp -> BExp)
opAnd = reservedOp "&&" >> return (BBinExp And)
opOr = reservedOp "||" >> return (BBinExp Or)

opNeg :: Parser (BExp -> BExp)
opNeg = reservedOp "!" >> return BNeg

bTrue, bFalse :: Parser BExp
bTrue = reserved "true" >> return  BTrue
bFalse = reserved "false" >> return  BFalse