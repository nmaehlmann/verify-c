module Parser.ArithmeticExpression (aExpC0, aExpFO) where
    
import Text.Parsec.String (Parser)

import AST
import qualified Parser.InternalArithmeticExpression as IAExp
import Parser.LExpression

aExpC0 :: Parser (AExp' C0)
aExpC0 = IAExp.aExpC0 lExpC0

aExpFO :: Parser (AExp' FO)
aExpFO = IAExp.aExpFO lExpFO


