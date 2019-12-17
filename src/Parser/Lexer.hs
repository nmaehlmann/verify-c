module Parser.Lexer where
    
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec
import Data.List

operators :: [String]
operators = ["+", "-", "*", "/", "=", "<", "==", "&&", "||", "[", "]"]

operatorSymbols :: [Char]
operatorSymbols = nub $ mconcat operators

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
    , Tok.reservedNames   = ["if", "else", "while", "return", "true", "false"]
    , Tok.reservedOpNames = operators
    , Tok.caseSensitive   = True
    }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

lIdentifier :: Parser String
lIdentifier = Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

parens :: Parser p -> Parser p
parens = Tok.parens lexer

braces :: Parser p -> Parser p
braces = Tok.braces lexer

whiteSpace ::  Parser ()
whiteSpace = Tok.whiteSpace lexer