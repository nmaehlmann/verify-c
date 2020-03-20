module Parser.LExpression (lExpC0, lExpFO) where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import qualified Parser.Lexer as Lex
import Parser.Identifier
import Parser.InternalArithmeticExpression

lExpC0 :: Parser (LExpr' C0)
lExpC0 = buildExpressionParser lOperatorTableC0 lTerm

lExpFO :: Parser (LExpr' FO)
lExpFO = buildExpressionParser lOperatorTableFO lTerm

lTerm :: Parser (LExpr' l)
lTerm = lIdentifier

lIdentifier :: Parser (LExpr' l)
lIdentifier = LIdt <$> identifier

lOperatorTableC0 :: OperatorTable String () Identity (LExpr' C0)
lOperatorTableC0 = 
    [ [postfix (opArray (aExpC0 lExpC0))]
    , [prefix opDereference]
    , [postfix opPart]
    ]

lOperatorTableFO :: OperatorTable String () Identity (LExpr' FO)
lOperatorTableFO = 
    [ [postfix (opArray (aExpFO lExpFO))]
    , [postfix opPart]
    ]

opPart :: Parser (LExpr' l -> LExpr' l)
opPart = do
    Lex.reservedOp "."
    part <- identifier
    return $ \s -> LStructurePart s part

-- TODO: why does this not work with reservedOp?
opDereference :: Parser (LExpr' C0 -> LExpr' C0)
opDereference = char '*' >> return LDeref

opArray :: Parser (AExpr' l) -> Parser (LExpr' l -> LExpr' l)
opArray aExp = do
    idx <- Lex.brackets $ aExp
    return $ (\lExpression -> LArray lExpression idx)

-- prefix & postfix copied from: https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported    
-- prefix :: ParsecT s u m (a -> a) -> Operator s u m a
prefix, postfix :: Stream s m t => ParsecT s u m (a -> a) -> Operator s u m a
prefix  p = Prefix  . chainl1 p $ return       (.)
postfix p = Postfix . chainl1 p $ return (flip (.))