module AST where

data Idt = Idt String
    deriving (Eq, Show)

data LExp 
    = LIdt String 
    | LArray LExp AExp
    | LStructPart LExp LExp
    | LDereference LExp
    deriving (Eq, Show)    

data AExp 
    = ALit Integer
    | AIdt LExp
    | ABinExp BinOp AExp AExp
    | AAddress LExp -- is this useful in c0?
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

data Stmt 
    = Assignment LExp AExp
    | ITE BExp Stmt Stmt
    | While BExp Stmt
    | Seq Stmt Stmt
    | Empty
    deriving (Eq, Show)