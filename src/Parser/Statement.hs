module Parser.Statement (statement) where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (Empty)

import AST
import Parser.Lexer
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.LExpression
import Parser.Type

statement :: Parser Stmt
statement = whiteSpace >> chainl singleStatement (return Seq) Empty

singleStatement :: Parser Stmt
singleStatement = declAssignement <|> assignment <|> ifThenElse <|> while <|> returnStatement <|> assertion

assertion :: Parser Stmt
assertion = Assertion <$> assertionFO "assertion"

invariant :: Parser (BExp' FO)
invariant = assertionFO "invariant"

declAssignement :: Parser Stmt
declAssignement = do
    typeName
    a@(Assignment idt _) <- assignment
    return $ Seq (Declaration idt) a

assignment :: Parser Stmt
assignment = do
    idt <- lExpC0
    reservedOp "="
    expr <- aExpC0
    semi
    return $ Assignment idt expr

ifThenElse :: Parser Stmt
ifThenElse = do
    reserved "if"
    condition <- parens bExpC0
    ifCase <- braces statement
    reserved "else"
    elseCase <- braces statement
    return $ ITE condition ifCase elseCase

while :: Parser Stmt
while = do
    reserved "while"
    condition <- parens bExpC0
    (inv, body) <- braces $ do
        inv <- invariant
        body <- statement
        return (inv, body)
    return $ While condition inv body
    
returnStatement :: Parser Stmt
returnStatement = do
    reserved "return"
    returnValue <- optionMaybe aExpC0
    semi
    return $ Return returnValue