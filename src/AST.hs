{-# LANGUAGE DeriveFunctor #-}
module AST where

data BExp 
    = BTrue
    | BFalse
    | BComp CompOp AExp AExp
    | BNeg BExp
    | BBinExp BBinOp BExp BExp
    deriving (Eq, Show)

data FO a
    = FOTrue
    | FOFalse
    | FOComp CompOp a a
    | FONeg (FO a)
    | FOBinExp BBinOp (FO a) (FO a)
    | Forall Idt (FO a)
    | Exists Idt (FO a)
    | Predicate Idt [a]
    deriving (Eq, Show, Functor)

type FOExp = FO AExp

type FOSExp = FO ASExp

data LExpression a
    = LIdt Idt
    | LArray (LExpression a) a
    | LStructPart (LExpression a) (LExpression a)
    | LDereference (LExpression a)
    deriving (Eq, Show, Functor)

type LExp = LExpression AExp

type LSExp = LExpression ASExp

data ArithemticExpression v 
    = ALit Integer
    | AIdt v
    | ABinExp ABinOp (ArithemticExpression v) (ArithemticExpression v)
    | AArray [ArithemticExpression v]
    | AFunCall Idt [ArithemticExpression v]
    deriving (Eq, Show, Functor)

type AExp = ArithemticExpression ReadLExp

data ReadLExp = ReadLExp LExp
    deriving (Eq, Show)

type ASExp = ArithemticExpression SReadLExp

data SReadLExp = SReadLExp State LSExp
    deriving (Eq, Show)

data State
    = Atomic String
    | Update State LExp AExp
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