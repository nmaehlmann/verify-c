module Parser.InternalArithmeticExpression (aExp) where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.Lexer

aExp :: Parser LExp -> Parser AExp
aExp lExpParser = buildExpressionParser aOperatorTable $ aTerm lExpParser

aTerm :: Parser LExp -> Parser AExp
aTerm lExpParser = parens (aExp lExpParser) <|> aLit <|> aLExp lExpParser

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

aLExp :: Parser LExp -> Parser AExp
aLExp = fmap AIdt 