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
assertion = Assertion <$> fOAssertion "assertion"

precondition :: Parser FOExp
precondition = fOAssertion "precondition"

postcondition :: Parser FOExp
postcondition = fOAssertion "postcondition"

invariant :: Parser FOExp
invariant = fOAssertion "invariant"

fOAssertion :: String -> Parser FOExp
fOAssertion keyword = do
    reserved keyword
    fo <- parens (quotes fOExp)
    semi
    return fo

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

funDef :: Parser Stmt
funDef = do
    returnType <- typeName
    name <- identifier
    arguments <- parens $ declaration `sepBy` comma
    (pre, post, body) <- braces $ do 
        pre <- precondition 
        post <- postcondition
        body <- statement
        return (pre, post, body)
    return $ FunDef $ FunctionDefinition
        { funDefType     = returnType
        , funDefName     = name
        , funDefArgs     = arguments
        , funDefPrecond  = pre
        , funDefPostcond = post
        , funDefBody     = body
        }

    