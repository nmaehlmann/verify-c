module Parser.LExpression (lExp) where

import Data.Functor.Identity
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import qualified Parser.Lexer as Lex
import Parser.Identifier
import Parser.InternalArithmeticExpression

lExp :: Parser LExp
lExp = buildExpressionParser lOperatorTable lTerm

lTerm :: Parser LExp
lTerm = lIdentifier

lIdentifier :: Parser LExp
lIdentifier = LIdt <$> identifier

lOperatorTable :: OperatorTable String () Identity LExp
lOperatorTable = 
    [ [postfix opArray]
    , [prefix opDereference]
    , [Infix opPart AssocLeft]
    ]

opPart :: Parser (LExp -> LExp -> LExp)
opPart = Lex.reservedOp "." >> return LStructPart

-- TODO: why does this not work with reservedOp?
opDereference :: Parser (LExp -> LExp)
opDereference = char '*' >> return LDereference

opArray :: Parser (LExp -> LExp)
opArray = do
    idx <- Lex.brackets $ aExp lExp
    return $ (\lExpression -> LArray lExpression idx)

-- prefix & postfix copied from: https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported    
-- prefix :: ParsecT s u m (a -> a) -> Operator s u m a
prefix, postfix :: Stream s m t => ParsecT s u m (a -> a) -> Operator s u m a
prefix  p = Prefix  . chainl1 p $ return       (.)
postfix p = Postfix . chainl1 p $ return (flip (.))