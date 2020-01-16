module LogicExpression where

import AST

class LogicExpression a where
    true :: a
    false :: a
    comparison :: CompOp -> AExp -> AExp -> a
    negation :: a -> a
    binaryExpression :: BBinOp -> a -> a -> a

instance LogicExpression BExp where
    true = BTrue
    false = BFalse
    comparison = BComp
    negation = BNeg
    binaryExpression = BBinExp

instance LogicExpression FOExp where
    true = FOTrue
    false = FOFalse
    comparison = FOComp
    negation = FONeg
    binaryExpression = FOBinExp
