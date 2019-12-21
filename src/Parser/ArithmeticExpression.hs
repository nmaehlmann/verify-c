module Parser.ArithmeticExpression (aExp) where
    
import Text.Parsec.String (Parser)

import AST
import qualified Parser.InternalArithmeticExpression as IAExp
import Parser.LExpression

aExp :: Parser AExp
aExp = IAExp.aExp lExp

