module Parser.InternalArithmeticExpression (aExpC0, aExpFO) where

import Data.Functor.Identity
import Control.Monad
import Data.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec

import AST
import Parser.Identifier
import Parser.Lexer

aExpC0 :: Parser (LExp C0 Plain) -> Parser (AExp C0 Plain)
aExpC0 lExpParser = buildExpressionParser aOperatorTable $ aTermC0 lExpParser

aTermC0 :: Parser (LExp C0 Plain) -> Parser (AExp C0 Plain)
aTermC0 lExpParser = parens (aExpC0 lExpParser) 
    <|> aLit 
    <|> aLExp lExpParser

aExpFO :: Parser (LExp FO Plain) -> Parser (AExp FO Plain)
aExpFO lExpParser = buildExpressionParser aOperatorTable $ aTermFO lExpParser

aTermFO :: Parser (LExp FO Plain) -> Parser (AExp FO Plain)
aTermFO lExpParser = parens (aExpFO lExpParser) 
    <|> aLogVar
    <|> funCall (aExpFO lExpParser)
    <|> aLit 
    <|> aLExp lExpParser

aOperatorTable :: OperatorTable String () Identity (AExp l m)
aOperatorTable =
    [ [Infix opMul AssocLeft, Infix opDiv AssocLeft]
    , [Infix opAdd AssocLeft, Infix opSub AssocLeft]
    ]

opMul, opDiv, opAdd, opSub :: Parser (AExp l m -> AExp l m -> AExp l m)
opMul = reservedOp "*" >> return (ABinExp Mul)
opDiv = reservedOp "/" >> return (ABinExp Div)
opAdd = reservedOp "+" >> return (ABinExp Add)
opSub = reservedOp "-" >> return (ABinExp Sub)

aLit :: Parser (AExp l m)
aLit = ALit <$> integer

aLogVar :: Parser (AExp FO m)
aLogVar = try $ do
    (Idt str) <- identifier
    when (not (startsWithUpperCase str)) $ fail $ "Logic variable " ++ str ++ " is supposed to start with a capital letter."
    return $ ALogVar $ Idt str

startsWithUpperCase :: String -> Bool
startsWithUpperCase (h:_) = isUpper h
startsWithUpperCase _ = False

aLExp :: Parser (LExp l Plain) -> Parser (AExp l Plain)
aLExp = fmap AIdt

funCall :: Parser (AExp FO m) ->  Parser (AExp FO m)
funCall aExpParser = try $ do
    funName <- identifier
    funArgs <- parens $ commaSep aExpParser
    return $ AFunCall funName funArgs