module Lexer where
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec

operatorSymbols :: [Char]
operatorSymbols = "+-*/=[]"

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
    { Tok.commentStart    = "/*"
    , Tok.commentEnd      = "*/"
    , Tok.commentLine     = "//"
    , Tok.nestedComments  = False
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = oneOf operatorSymbols
    , Tok.opLetter        = oneOf operatorSymbols
    , Tok.reservedNames   = ["if", "else", "while", "return"]
    , Tok.reservedOpNames = ["+", "-", "*", "/", "="]
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser p -> Parser p
parens = Tok.parens lexer