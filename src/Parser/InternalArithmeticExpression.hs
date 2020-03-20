module Parser.InternalArithmeticExpression (aExp) where

import Data.Functor.Identity
import Control.Monad
import Data.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.Identifier
import Parser.Lexer

aExp :: Parser LExp -> Parser AExp
aExp lExpParser = buildExpressionParser aOperatorTable $ aTerm lExpParser

aTerm :: Parser LExp -> Parser AExp
aTerm lExpParser = parens (aExp lExpParser) <|> aLogVar <|> aLit <|> aArray lExpParser <|> try (funCall lExpParser) <|> aLExp lExpParser

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

aLogVar :: Parser AExp
aLogVar = try $ do
    (Idt str) <- identifier
    when (startsWithLowerCase str) $ fail $ "Logic variable " ++ str ++ " is supposed to start with a capital letter."
    return $ ALogVar $ Idt str

startsWithLowerCase :: String -> Bool
startsWithLowerCase (h:_) = isLower h
startsWithLowerCase _ = False

aLExp :: Parser LExp -> Parser AExp
aLExp = fmap AIdt

aArray :: Parser LExp -> Parser AExp
aArray lExpParser = do 
    fields <- braces $ aExp lExpParser `sepBy` comma
    return $ AArray fields

funCall :: Parser LExp ->  Parser AExp
funCall lExpParser = do
    funName <- identifier
    funArgs <- parens $ commaSep $ aExp lExpParser
    return $ AFunCall funName funArgs