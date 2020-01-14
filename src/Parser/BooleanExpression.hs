module Parser.BooleanExpression (bExp) where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import LogicExpression
import Parser.ArithmeticExpression
import Parser.Identifier
import Parser.Lexer

bExp :: Parser BExp
bExp = buildExpressionParser bOperatorTable bTerm

bTerm :: Parser BExp
bTerm = parens bExp <|> bTrue <|> bFalse <|> bComparison

fOExp :: Parser FOExp
fOExp = parens fOExp <|> bTrue <|> bFalse <|> bComparison <|> forall <|> exists

forall :: Parser FOExp
forall = do
    reserved "forall"
    idt <- identifier
    Forall idt <$> fOExp

exists :: Parser FOExp
exists = do
    reserved "exists"
    idt <- identifier
    Exists idt <$> fOExp

bComparison :: LogicExpression a => Parser a
bComparison = do
    lhs <- aExp
    op  <- comparisonOperator
    rhs <- aExp
    return $ comparison op lhs rhs

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

bOperatorTable :: LogicExpression a => OperatorTable String () Identity a
bOperatorTable =
    [ [Prefix opNeg]
    , [Infix opAnd AssocLeft, Infix opOr AssocLeft, Infix opImplies AssocLeft]
    ]

opAnd, opOr, opImplies :: LogicExpression a => Parser (a -> a -> a)
opAnd = reservedOp "&&" >> return (binaryExpression And)
opOr = reservedOp "||" >> return (binaryExpression Or)
opImplies = reservedOp "->" >> return (binaryExpression Implies)

opNeg :: LogicExpression a => Parser (a -> a)
opNeg = reservedOp "!" >> return negation

bTrue, bFalse :: LogicExpression a => Parser a
bTrue = reserved "true" >> return true
bFalse = reserved "false" >> return false