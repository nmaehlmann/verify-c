module Parser.Statement (statement) where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (Empty)

import AST
import Parser.Lexer
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.LExpression
import Parser.Type
import Parser.Identifier

statement :: Parser Stmt
statement = whiteSpace >> chainl singleStatement (return Seq) Empty

singleStatement :: Parser Stmt
singleStatement = declAssignement 
    <|> declAssignFunCall
    <|> assignFunCall
    <|> voidFunCall
    <|> decl 
    <|> assignment 
    <|> ifThenElse 
    <|> while 
    <|> returnStatement 
    <|> assertion

assertion :: Parser Stmt
assertion = Assertion <$> assertionFO "assertion"

invariant :: Parser (BExp' FO)
invariant = assertionFO "invariant"

voidFunCall :: Parser Stmt
voidFunCall = try $ do
    funName <- identifier
    funArgs <- parens $ commaSep aExpC0
    semi
    return $ FunCall Nothing funName funArgs

declAssignFunCall :: Parser Stmt
declAssignFunCall = try $ do
    typeName
    idt <- lExpC0
    reservedOp "="
    funName <- identifier
    funArgs <- parens $ commaSep aExpC0
    semi
    return $  Seq (Declaration idt) (FunCall (Just idt) funName funArgs)

assignFunCall :: Parser Stmt
assignFunCall = try $ do
    idt <- lExpC0
    reservedOp "="
    funName <- identifier
    funArgs <- parens $ commaSep aExpC0
    semi
    return $ FunCall (Just idt) funName funArgs

decl :: Parser Stmt
decl = do
    typeName
    idt <- lExpC0
    semi
    return $ Declaration idt

declAssignement :: Parser Stmt
declAssignement = try $ do
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