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

aExpC0 :: Parser (LExpr C0 Plain) -> Parser (AExpr C0 Plain)
aExpC0 lExpParser = buildExpressionParser aOperatorTable $ aTermC0 lExpParser

aTermC0 :: Parser (LExpr C0 Plain) -> Parser (AExpr C0 Plain)
aTermC0 lExpParser = parens (aExpC0 lExpParser) 
    <|> aLit 
    <|> aArray (aExpC0 lExpParser) 
    <|> aLExp lExpParser

aExpFO :: Parser (LExpr FO Plain) -> Parser (AExpr FO Plain)
aExpFO lExpParser = buildExpressionParser aOperatorTable $ aTermFO lExpParser

aTermFO :: Parser (LExpr FO Plain) -> Parser (AExpr FO Plain)
aTermFO lExpParser = parens (aExpFO lExpParser) 
    <|> aLogVar
    <|> funCall (aExpFO lExpParser)
    <|> aLit 
    <|> aArray (aExpFO lExpParser)
    <|> aLExp lExpParser

aOperatorTable :: OperatorTable String () Identity (AExpr l m)
aOperatorTable =
    [ [Infix opMul AssocLeft, Infix opDiv AssocLeft]
    , [Infix opAdd AssocLeft, Infix opSub AssocLeft]
    ]

opMul, opDiv, opAdd, opSub :: Parser (AExpr l m -> AExpr l m -> AExpr l m)
opMul = reservedOp "*" >> return (ABinExp Mul)
opDiv = reservedOp "/" >> return (ABinExp Div)
opAdd = reservedOp "+" >> return (ABinExp Add)
opSub = reservedOp "-" >> return (ABinExp Sub)

aLit :: Parser (AExpr l m)
aLit = ALit <$> integer

aLogVar :: Parser (AExpr FO m)
aLogVar = try $ do
    (Idt str) <- identifier
    when (startsWithLowerCase str) $ fail $ "Logic variable " ++ str ++ " is supposed to start with a capital letter."
    return $ ALogVar $ Idt str

startsWithLowerCase :: String -> Bool
startsWithLowerCase (h:_) = isLower h
startsWithLowerCase _ = False

aLExp :: Parser (LExpr l Plain) -> Parser (AExpr l Plain)
aLExp = fmap AIdt

aArray :: Parser (AExpr l m) -> Parser (AExpr l m)
aArray aExprParser = do 
    fields <- braces $ aExprParser `sepBy` comma
    return $ AArray fields

funCall :: Parser (AExpr FO m) ->  Parser (AExpr FO m)
funCall aExprParser = try $ do
    funName <- identifier
    funArgs <- parens $ commaSep aExprParser
    return $ AFunCall funName funArgs