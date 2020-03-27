module Parser.BooleanExpression (bExpC0, bExpFO, assertionFO) where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.ArithmeticExpression
import Parser.Identifier
import Parser.Lexer

bExpC0 :: Parser (BExp' C0)
bExpC0 = buildExpressionParser bOperatorTable bTermC0

bTermC0 :: Parser (BExp' C0)
bTermC0 = parens bExpC0 <|> bTrue <|> bFalse <|> bComparison aExpC0

bExpFO :: Parser (BExp' FO)
bExpFO = buildExpressionParser bOperatorTable bTermFO

bTermFO :: Parser (BExp' FO)
bTermFO = parens bExpFO <|> bTrue <|> bFalse <|> bComparison aExpFO <|> forall <|> exists <|> predicate

forall :: Parser (BExp' FO)
forall = quantifier "forall" BForall

exists :: Parser (BExp' FO)
exists = quantifier "exists" BExists

quantifier :: String -> (Idt -> BExp' FO -> BExp' FO) -> Parser (BExp' FO)
quantifier quantifierString quantifierConstructor = do
    reserved quantifierString
    (idt, fo) <- parens $ do
        idt <- identifier
        _ <- comma
        fo <- bExpFO
        return (idt, fo)
    return $ quantifierConstructor idt fo

bComparison :: Parser (AExp' l) -> Parser (BExp' l)
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

bOperatorTable :: OperatorTable String () Identity (BExp' l)
bOperatorTable =
    [ [Prefix opNeg]
    , [Infix opAnd AssocLeft, Infix opOr AssocLeft, Infix opImplies AssocLeft, Infix opIff AssocLeft]
    ]

opAnd, opOr, opImplies :: Parser (BExp' l -> BExp' l -> BExp' l)
opAnd = reservedOp "&&" >> return (BBinExp And)
opOr = reservedOp "||" >> return (BBinExp Or)
opImplies = reservedOp "->" >> return (BBinExp Implies)
opIff = reservedOp "<->" >> return (BBinExp Iff)

opNeg :: Parser (BExp' l -> BExp' l)
opNeg = reservedOp "!" >> return BNeg

bTrue, bFalse :: Parser (BExp' l)
bTrue = reserved "true" >> return BTrue
bFalse = reserved "false" >> return BFalse

predicate :: Parser (BExp' FO)
predicate = do
    predName <- identifier
    predArgs <- parens $ commaSep aExpFO
    return $ BPredicate predName predArgs

assertionFO :: String -> Parser (BExp' FO)
assertionFO keyword = do
    reserved keyword
    fo <- parens (quotes bExpFO)
    semi
    return fo