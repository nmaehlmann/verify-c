module Parser.LExpression where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec
import Text.Parsec.Char

import AST
import Parser.Lexer
import Parser.InternalArithmeticExpression

lExp :: Parser LExp
lExp = buildExpressionParser lOperatorTable lTerm

lTerm :: Parser LExp
lTerm = identifier

identifier :: Parser LExp
identifier = LIdt <$> lIdentifier

lOperatorTable :: OperatorTable String () Identity LExp
lOperatorTable = 
    [ [postfix arrayOp]
    , [prefix dereferenceOp]
    ]

dereferenceOp :: Parser (LExp -> LExp)
dereferenceOp = do
    char '*' --TODO: why does this not work with reservedOp?
    return $ LDereference


arrayOp :: Parser (LExp -> LExp)
arrayOp = do
    idx <- brackets $ aExp lExp
    return $ (\lExpression -> LArray lExpression idx)

-- prefix & postfix copied from: https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported    
prefix  p = Prefix  . chainl1 p $ return       (.)
postfix p = Postfix . chainl1 p $ return (flip (.))