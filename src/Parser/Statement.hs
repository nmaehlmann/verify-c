module Parser.Statement where

import Text.Parsec.String (Parser)
import Text.Parsec

import AST
import Parser.Lexer
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.LExpression

statement :: Parser Stmt
statement = whiteSpace >> chainl1 singleStatement (return Seq)

singleStatement :: Parser Stmt
singleStatement = assignment <|> ifThenElse <|> while

assignment :: Parser Stmt
assignment = do
    idt <- identifier
    reservedOp "="
    expr <- aExp
    reserved ";"
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
    
    