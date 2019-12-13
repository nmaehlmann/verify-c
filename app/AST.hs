module AST where

data Idt = Idt String
    deriving (Eq, Show)

data AExp 
    = ALit Int
    | AIdt Idt
    | ABinExp BinOp AExp AExp
    deriving (Eq, Show)    

data BinOp = Add | Sub | Mul | Div
    deriving (Eq, Show)

data BExp 
    = BTrue
    | BFalse
    | BEq AExp AExp
    | BLess AExp AExp
    | BNeg BExp
    | BAnd BExp BExp
    | BOr BExp BExp
    deriving (Eq, Show)    

data Exp = AExp | BExp
    deriving (Eq, Show)

data Stmt 
    = Assignment Idt AExp
    | ITE BExp Stmt Stmt
    | While BExp Stmt
    | Seq Stmt Stmt
    | Empty
    deriving (Eq, Show)