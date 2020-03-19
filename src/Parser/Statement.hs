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
assertion = Assertion <$> fOAssertion "assertion"

invariant :: Parser FOExp
invariant = fOAssertion "invariant"

declAssignement :: Parser Stmt
declAssignement = do
    typeName
    a@(Assignment idt _) <- assignment
    return $ Seq (Declaration idt) a

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
    (inv, body) <- braces $ do
        inv <- invariant
        body <- statement
        return (inv, body)
    return $ While condition inv body
    
returnStatement :: Parser Stmt
returnStatement = do
    reserved "return"
    returnValue <- optionMaybe aExp
    semi
    return $ Return returnValue