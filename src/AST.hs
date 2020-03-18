{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AST where
import Data.List

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
    deriving (Eq, Functor)

type FOExp = FO AExp
type FOSExp = FO ASExp

data LExp
    = LIdt Idt
    | LArray LExp AExp
    | LStructPart LExp Idt
    | LDereference LExp
    deriving (Eq, Show)

data LSExp
    = LSIdt Idt
    | LSArray LSExp ASExp
    | LSStructPart LSExp Idt
    | LSRead ReadLExp
    deriving (Eq, Ord)

data AExp 
    = ALit Integer
    | AIdt LExp
    | ABinExp ABinOp AExp AExp
    | AArray [AExp]
    | AFunCall Idt [AExp]
    | ALogVar Idt
    deriving (Eq, Show)

data ASExp 
    = ASLit Integer
    | ASRead ReadLExp
    | ASBinExp ABinOp ASExp ASExp
    | ASArray [ASExp]
    | ASFunCall Idt [ASExp]
    | ASLogVar Idt
    deriving (Eq, Ord)

data ReadLExp = ReadLExp State LSExp
    deriving (Eq, Ord)

data State
    = Atomic String
    | Update State LSExp ASExp
    deriving (Eq, Ord)

data ABinOp = Add | Sub | Mul | Div
    deriving (Eq, Ord)

data CompOp 
    = Less 
    | LessOrEqual 
    | Greater 
    | GreaterOrEqual 
    | Equal 
    | NotEqual
    deriving (Eq)

data BBinOp
    = And
    | Or
    | Implies
    deriving (Eq)

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
    deriving (Eq, Ord)

data Decl = Decl Type Idt
    deriving (Eq, Show)

instance (Show a) => Show (FO a) where
    show FOTrue = "true"
    show FOFalse = "false"
    show (FOComp op l r) = showBinExp op l r
    show (FONeg f) = "!(" ++ show f ++ ")"
    show (FOBinExp op l r) = showBinExp op l r
    show (Forall i f) = "forall(" ++ show i ++ ", " ++ show f ++ ")"
    show (Exists i f) = "exists(" ++ show i ++ ", " ++ show f ++ ")"
    show (Predicate name args) = show name ++ "(" ++ argsList ++ ")"
        where argsList = concat $ intersperse "," $ map show args

showBinExp :: (Show a, Show o) => o -> a -> a -> String
showBinExp op l r = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

instance Show Idt where
    show (Idt s) = s

instance Show ASExp where
    show (ASLit i) = show i
    show (ASRead readLExp) = show readLExp
    show (ASBinExp op l r) = showBinExp op l r
    show (ASArray fields) = "[" ++ showFields ++ "]" 
        where showFields = concat $ intersperse "," $ map show fields
    show (ASFunCall name args) = show name ++ "(" ++ argsList ++ ")"
        where argsList = concat $ intersperse "," $ map show args
    show (ASLogVar i) = show i

instance Show ABinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show ReadLExp where
    -- show (ReadLExp (Atomic _) (LSIdt i)) = show i
    show (ReadLExp state lSExp) = "read(" ++ show state ++ ", " ++ show lSExp ++ ")"

instance Show LSExp where
    show (LSIdt i) = show i
    show (LSArray lSExp aSExp) = show lSExp ++ "[" ++ show aSExp ++ "]"
    show (LSStructPart lSExp idt) = show lSExp ++ "." ++ show idt
    show (LSRead r) = show r

instance Show State where
    show (Atomic s) = s
    show (Update state lSExp aSExp) = "upd(" 
        ++ show state ++ ", " 
        ++ show lSExp ++ ", "
        ++ show aSExp ++ ")"

instance Show BBinOp where
    show And = "&&"
    show Or = "||"
    show Implies = "->"

instance Show CompOp where
    show Less = "<"
    show LessOrEqual = "<=" 
    show Greater = ">" 
    show GreaterOrEqual = ">="
    show Equal = "="
    show NotEqual = "!="

sigma :: State
sigma = Atomic "s"