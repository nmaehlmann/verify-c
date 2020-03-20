module Parser.BooleanExpression (bExpC0, bExpFO, assertionFO) where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.ArithmeticExpression
import Parser.Identifier
import Parser.Lexer

bExpC0 :: Parser (BExpr' C0)
bExpC0 = buildExpressionParser bOperatorTable bTermC0

bTermC0 :: Parser (BExpr' C0)
bTermC0 = parens bExpC0 <|> bTrue <|> bFalse <|> bComparison aExpC0

bExpFO :: Parser (BExpr' FO)
bExpFO = buildExpressionParser bOperatorTable bTermFO

bTermFO :: Parser (BExpr' FO)
bTermFO = parens bTermFO <|> bTrue <|> bFalse <|> bComparison aExpFO <|> forall <|> exists <|> predicate

forall :: Parser (BExpr' FO)
forall = quantifier "forall" BForall

exists :: Parser (BExpr' FO)
exists = quantifier "exists" BExists

quantifier :: String -> (Idt -> BExpr' FO -> BExpr' FO) -> Parser (BExpr' FO)
quantifier quantifierString quantifierConstructor = do
    reserved quantifierString
    (idt, fo) <- parens $ do
        idt <- identifier
        _ <- comma
        fo <- bExpFO
        return (idt, fo)
    return $ quantifierConstructor idt fo

bComparison :: Parser (AExpr' l) -> Parser (BExpr' l)
bComparison aExp = try $ do
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

bOperatorTable :: OperatorTable String () Identity (BExpr' l)
bOperatorTable =
    [ [Prefix opNeg]
    , [Infix opAnd AssocLeft, Infix opOr AssocLeft, Infix opImplies AssocLeft]
    ]

opAnd, opOr, opImplies :: Parser (BExpr' l -> BExpr' l -> BExpr' l)
opAnd = reservedOp "&&" >> return (BBinExp And)
opOr = reservedOp "||" >> return (BBinExp Or)
opImplies = reservedOp "->" >> return (BBinExp Implies)

opNeg :: Parser (BExpr' l -> BExpr' l)
opNeg = reservedOp "!" >> return BNeg

bTrue, bFalse :: Parser (BExpr' l)
bTrue = reserved "true" >> return BTrue
bFalse = reserved "false" >> return BFalse

predicate :: Parser (BExpr' FO)
predicate = do
    predName <- identifier
    predArgs <- parens $ commaSep aExpFO
    return $ BPredicate predName predArgs

assertionFO :: String -> Parser (BExpr' FO)
assertionFO keyword = do
    reserved keyword
    fo <- parens (quotes bExpFO)
    semi
    return fo