module Parser.Type (typeName) where

import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr

import AST
import Parser.Lexer

typeName :: Parser Type
typeName = buildExpressionParser tOperatorTable tTerm

tTerm :: Parser Type
tTerm = tVoid <|> tInt <|> tStruct

tInt :: Parser Type
tInt = reserved "int" >> return TInt

tVoid :: Parser Type
tVoid = reserved "void" >> return TVoid

tStruct :: Parser Type
tStruct = TStruct <$> lIdentifier

tOperatorTable :: OperatorTable String () Identity Type
tOperatorTable = 
    [ [postfix opArray]
    , [postfix opReference]
    ]

opArray :: Parser (Type -> Type)
opArray = reserved "[]" >> return TReference

opReference :: Parser (Type -> Type)
opReference = reserved "*" >> return TReference

-- prefix & postfix copied from: https://stackoverflow.com/questions/10475337/parsec-expr-repeated-prefix-postfix-operator-not-supported    
prefix, postfix :: Stream s m t => ParsecT s u m (a -> a) -> Operator s u m a
prefix  p = Prefix  . chainl1 p $ return       (.)
postfix p = Postfix . chainl1 p $ return (flip (.))