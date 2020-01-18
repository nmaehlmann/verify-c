module Parser.BooleanExpression (bExp, fOExp) where

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
fOExp = buildExpressionParser bOperatorTable fOTerm

fOTerm :: Parser FOExp
fOTerm = parens fOExp <|> bTrue <|> bFalse <|> bComparison <|> forall <|> exists <|> predicate

forall :: Parser FOExp
forall = quantifier "forall" Forall

exists :: Parser FOExp
exists = quantifier "exists" Exists

quantifier :: String -> (Idt -> FOExp -> FOExp) -> Parser FOExp
quantifier quantifierString quantifierConstructor = do
    reserved quantifierString
    (idt, fo) <- parens $ do
        idt <- identifier
        _ <- comma
        fo <- fOExp
        return (idt, fo)
    return $ quantifierConstructor idt fo

bComparison :: LogicExpression a => Parser a
bComparison = try $ do
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

predicate :: Parser FOExp
predicate = do
    predName <- identifier
    predArgs <- parens $ commaSep aExp
    return $ Predicate predName predArgs