module AST where

data LExp 
    = LIdt Idt 
    | LArray LExp AExp
    | LStructPart LExp LExp
    | LDereference LExp
    deriving (Eq, Show)    

data AExp 
    = ALit Integer
    | AIdt LExp
    | ABinExp BinOp AExp AExp
    | AAddress LExp -- is this useful in c0?
    | AArray [AExp]
    | AFunCall [AExp]
    deriving (Eq, Show)    

data BinOp = Add | Sub | Mul | Div
    deriving (Eq, Show)

data CompOp 
    = Less 
    | LessOrEqual 
    | Greater 
    | GreaterOrEqual 
    | Equal 
    | NotEqual
    deriving (Eq, Show)

data BExp 
    = BTrue
    | BFalse
    | BComp CompOp AExp AExp
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
    | FunDef Type Idt [Decl] Stmt
    | Return (Maybe AExp)
    | Assertion BExp 
    deriving (Eq, Show)

data Type
    = TInt
    | TChar
    | TStruct String
    | TArray Type
    | TReference Type
    | TVoid
    deriving (Eq, Show)

data Idt = Idt String
    deriving (Eq, Show)

data Decl = Decl Type Idt
    deriving (Eq, Show)

data Sentence 
    = STrue
    | SFalse
    | SComp CompOp AExp AExp
    | SNeg Sentence
    | SForAll Idt Sentence
    | SExists Idt Sentence
    | SPredicate [AExp]
    deriving (Eq, Show)

data Term
    = TLit Integer
    | TIdt LExp
    | TBinExp BinOp Term Term
    | TFunCall [Term]
    | TReadState State LExp
    deriving (Eq, Show)

data State
    = StateVar Idt
    | StateUpdate State LExp Term
    deriving (Eq, Show)