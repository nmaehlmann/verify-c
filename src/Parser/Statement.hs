module Parser.Statement where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (Empty)

import AST
import Parser.Lexer
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.Declaration
import Parser.LExpression
import Parser.Identifier
import Parser.Type

statement :: Parser Stmt
statement = whiteSpace >> chainl singleStatement (return Seq) Empty

singleStatement :: Parser Stmt
singleStatement = assignment <|> ifThenElse <|> while <|> funDef <|> returnStatement <|> assertion

assertion :: Parser Stmt
assertion = do
    reserved "/*w:"
    b <- bExp
    return $ Assertion b

assignment :: Parser Stmt
assignment = do
    idt <- lExp
    reservedOp "="
    expr <- aExp
    semi
    return $ Assignment idt expr

ifThenElse :: Parser Stmt
ifThenElse = do
    reserved "if"
    condition <- parens bExp
    ifCase <- braces statement
    reserved "else"
    elseCase <- braces statement
    return $ ITE condition ifCase elseCase

while :: Parser Stmt
while = do
    reserved "while"
    condition <- parens bExp
    body <- braces statement
    return $ While condition body
    
returnStatement :: Parser Stmt
returnStatement = do
    reserved "return"
    returnValue <- optionMaybe aExp
    semi
    return $ Return returnValue

funDef :: Parser Stmt
funDef = do
    returnType <- typeName
    name <- identifier
    arguments <- parens $ declaration `sepBy` comma
    body <- braces $ statement
    return $ FunDef returnType name arguments body

    