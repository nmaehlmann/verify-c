module Parser.Sentence where

-- import Data.Functor.Identity
-- import Text.Parsec.String (Parser)
-- import Text.Parsec.Expr
-- import Text.Parsec

-- import AST
-- import Parser.ArithmeticExpression
-- import Parser.Lexer
    
-- sentence :: Parser Sentence
-- sentence = buildExpressionParser sOperatorTable sTerm

-- sTerm :: Parser Sentence
-- sTerm = parens sExp <|> sTrue <|> sFalse <|> comparison

-- comparison :: Parser Sentence
-- comparison = do
--     lhs <- aExp
--     op  <- comparisonOperator
--     rhs <- aExp
--     return $ SComp op lhs rhs

-- comparisonOperator :: Parser CompOp
-- comparisonOperator = choice $ toParser <$> comparisonOperators
--     where toParser (txt, op) = try $ reservedOp txt >> return op

-- comparisonOperators :: [(String, CompOp)]
-- comparisonOperators = 
--     [ ("==", Equal)
--     , ("!=", NotEqual)
--     , ("<=", LessOrEqual)
--     , ("<" , Less)
--     , (">=", GreaterOrEqual)
--     , (">" , Greater)
--     ]

-- bOperatorTable :: OperatorTable String () Identity BExp
-- bOperatorTable =
--     [ [Prefix opNeg]
--     , [Infix opAnd AssocLeft, Infix opOr AssocLeft]
--     ]

-- opAnd, opOr :: Parser (BExp -> BExp -> BExp)
-- opAnd = reservedOp "&&" >> return (BBinExp And)
-- opOr = reservedOp "||" >> return (BBinExp Or)

-- opNeg :: Parser (BExp -> BExp)
-- opNeg = reservedOp "!" >> return BNeg

-- bTrue, bFalse :: Parser BExp
-- bTrue = reserved "true" >> return  BTrue
-- bFalse = reserved "false" >> return  BFalse