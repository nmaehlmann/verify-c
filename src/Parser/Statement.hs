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
singleStatement = try declAssignement 
    <|> try declAssignFunCall
    <|> assignFunCall
    <|> voidFunCall
    <|> try decl 
    <|> assignment 
    <|> ifThenElse 
    <|> while 
    <|> returnStatement 
    <|> assertion

assertion :: Parser Stmt
assertion = Assertion <$> assertionFO "assertion" <*> lineNo

invariant :: Parser (BExp' FO)
invariant = assertionFO "invariant"

voidFunCall :: Parser Stmt
voidFunCall = try $ do
    funName <- identifier
    funArgs <- parens $ commaSep aExpC0
    semi
    line <- lineNo
    return $ FunCall Nothing funName funArgs line

declAssignFunCall :: Parser Stmt
declAssignFunCall = try $ do
    typeName
    idt <- identifier
    reservedOp "="
    funName <- identifier
    funArgs <- parens $ commaSep aExpC0
    semi
    line <- lineNo
    return $  Seq (Declaration idt) (FunCall (Just (LIdt idt)) funName funArgs line)

assignFunCall :: Parser Stmt
assignFunCall = try $ do
    idt <- lExpC0
    reservedOp "="
    funName <- identifier
    funArgs <- parens $ commaSep aExpC0
    semi
    line <- lineNo
    return $ FunCall (Just idt) funName funArgs line

decl :: Parser Stmt
decl = do
    typeName
    idt <- identifier
    semi
    return $ Declaration idt

declAssignement :: Parser Stmt
declAssignement = try $ do
    typeName
    a@(Assignment (LIdt idt) _) <- assignment
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
    optionalElseCase <- optionMaybe $ do
        reserved "else"
        braces statement
    let elseCase = case optionalElseCase of
            (Just e) -> e
            (Nothing) -> Empty
    return $ ITE condition ifCase elseCase

while :: Parser Stmt
while = do
    reserved "while"
    condition <- parens bExpC0
    (line, inv, body) <- braces $ do
        line <- lineNo
        inv <- invariant
        body <- statement
        return (line, inv, body)
    return $ While condition inv body line
    
returnStatement :: Parser Stmt
returnStatement = do
    reserved "return"
    returnValue <- optionMaybe aExpC0
    semi
    return $ Return returnValue

lineNo :: Parser LineNo
lineNo = LineNo <$> sourceLine <$> getPosition 