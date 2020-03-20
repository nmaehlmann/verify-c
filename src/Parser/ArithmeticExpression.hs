module Parser.ArithmeticExpression (aExpC0, aExpFO) where
    
import Text.Parsec.String (Parser)

import AST
import qualified Parser.InternalArithmeticExpression as IAExp
import Parser.LExpression

aExpC0 :: Parser (AExpr' C0)
aExpC0 = IAExp.aExpC0 lExpC0

aExpFO :: Parser (AExpr' FO)
aExpFO = IAExp.aExpFO lExpFO


