module AST where

data BExp 
    = BTrue
    | BFalse
    | BComp CompOp AExp AExp
    | BNeg BExp
    | BBinExp BBinOp BExp BExp
    deriving (Eq, Show)

type FOExp = FO AExp

type FOSExp = FO ASExp

data FO a
    = FOTrue
    | FOFalse
    | FOComp CompOp a a
    | FONeg (FO a)
    | FOBinExp BBinOp (FO a) (FO a)
    | Forall Idt (FO a)
    | Exists Idt (FO a)
    | Predicate Idt [a]
    deriving (Eq, Show)

data LExp 
    = LIdt Idt 
    | LArray LExp AExp
    | LStructPart LExp LExp
    | LDereference LExp
    deriving (Eq, Show)    

data AExp 
    = ALit Integer
    | AIdt LExp
    | ABinExp ABinOp AExp AExp
    | AArray [AExp]
    | AFunCall Idt [AExp]
    deriving (Eq, Show)

data State
    = Atomic String
    | Update State LExp AExp
    deriving (Eq, Show)

data ASExp
    = ASLit Integer
    | ASRead State LExp
    | ASBinExp ABinOp ASExp ASExp
    | ASArray [ASExp]
    | ASFunCall Idt [ASExp]
    deriving (Eq, Show)

data ABinOp = Add | Sub | Mul | Div
    deriving (Eq, Show)

data CompOp 
    = Less 
    | LessOrEqual 
    | Greater 
    | GreaterOrEqual 
    | Equal 
    | NotEqual
    deriving (Eq, Show)

data BBinOp
    = And
    | Or
    | Implies
    deriving (Eq, Show)

data Stmt 
    = Assignment LExp AExp
    | ITE BExp Stmt Stmt
    | While BExp FOExp Stmt
    | Seq Stmt Stmt
    | Return (Maybe AExp)
    | Assertion FOExp 
    | Empty
    deriving (Eq, Show)

data Program = Program [FunctionDefinition]
    deriving (Eq, Show)

data FunctionDefinition = FunctionDefinition 
    { funDefType      :: Type
    , funDefName      :: Idt
    , funDefArgs      :: [Decl]
    , funDefPrecond   :: FOExp
    , funDefPostcond  :: FOExp
    , funDefBody      :: Stmt
    } deriving (Eq, Show)

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

