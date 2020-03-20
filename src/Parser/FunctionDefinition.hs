module Parser.FunctionDefinition (funDef) where

import Text.Parsec.String (Parser)
import Text.Parsec hiding (Empty)

import AST
import Parser.Declaration
import Parser.Identifier
import Parser.Type
import Parser.Statement
import Parser.Lexer
import Parser.BooleanExpression

funDef :: Parser FunctionDefinition
funDef = do
    returnType <- typeName
    name <- identifier
    arguments <- parens $ declaration `sepBy` comma
    (pre, post, body) <- braces $ do 
        pre <- precondition 
        post <- postcondition
        body <- statement
        return (pre, post, body)
    return $ FunctionDefinition
        { funDefType     = returnType
        , funDefName     = name
        , funDefArgs     = arguments
        , funDefPrecond  = pre
        , funDefPostcond = post
        , funDefBody     = body
        }

precondition :: Parser (BExpr' FO)
precondition = assertionFO "precondition"

postcondition :: Parser (BExpr' FO)
postcondition = assertionFO "postcondition"