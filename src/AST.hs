{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AST where
import Data.List

data State
    = Atomic String
    | Update State (LExp FO Refs) (AExp FO Refs)
    deriving (Eq, Ord)

data ABinOp = Add | Sub | Mul | Div
    deriving (Eq, Ord)

data CompOp = Less | LessOrEqual | Greater | GreaterOrEqual | Equal | NotEqual
    deriving (Eq)

data BBinOp = And | Or | Implies | Iff
    deriving (Eq)

newtype LineNo = LineNo Int deriving (Eq, Show)

type BExp' l = BExp l Plain
type AExp' l = AExp l Plain
type LExp' l = LExp l Plain

data Stmt 
    = Assignment (LExp' C0) (AExp' C0)
    | ITE (BExp' C0) Stmt Stmt
    | While (BExp' C0) (BExp' FO) Stmt LineNo
    | Seq Stmt Stmt
    | Return (Maybe (AExp' C0))
    | Assertion (BExp'  FO) LineNo
    | Declaration Idt
    | FunCall (Maybe (LExp' C0)) Idt [AExp' C0] LineNo
    | Empty
    deriving (Eq, Show)

data Program = Program [FunctionDefinition]
    deriving (Eq, Show)

data FunctionDefinition = FunctionDefinition 
    { funDefType      :: Type
    , funDefName      :: Idt
    , funDefArgs      :: [Decl]
    , funDefPrecond   :: BExp' FO
    , funDefPostcond  :: BExp' FO
    , funDefBody      :: Stmt
    }
    deriving (Eq, Show)

data Type
    = TInt
    | TStruct String
    | TArray Type
    | TReference Type
    | TVoid
    deriving (Eq, Show)

data Idt = Idt String
    deriving (Eq, Ord)

data Decl = Decl Type Idt
    deriving (Eq, Show)

data C0 = C0
data FO = FO
data Refs = Refs
data Plain = Plain

data BExp l m where
    BTrue       :: BExp l m
    BFalse      :: BExp l m
    BNeg        :: BExp l m -> BExp l m
    BBinExp     :: BBinOp -> BExp l m -> BExp l m -> BExp l m
    BComp       :: CompOp -> AExp l m -> AExp l m -> BExp l m
    BForall     :: Idt -> BExp FO m -> BExp FO m
    BExists     :: Idt -> BExp FO m -> BExp FO m
    BPredicate  :: Idt -> [AExp FO m] -> BExp FO m

data LExp l m where
    LIdt            :: Idt -> LExp l m
    LArray          :: LExp l m -> AExp l m -> LExp l m
    LStructurePart  :: LExp l m -> Idt -> LExp l m
    LRead           :: State -> LExp l Refs -> LExp l Refs
    LDeref          :: LExp l Plain -> LExp l Plain

data AExp l m where
    ALit        :: Integer -> AExp l m
    AIdt        :: LExp l m -> AExp l m
    ABinExp     :: ABinOp -> AExp l m -> AExp l m -> AExp l m
    AFunCall    :: Idt -> [AExp FO m] -> AExp FO m
    ALogVar     :: Idt -> AExp FO m
    AAddress    :: LExp l Plain -> AExp l Plain

mapAExps :: (AExp l m1 -> AExp FO m2) -> (BExp l m1) -> (BExp FO m2)
mapAExps _ BTrue = BTrue
mapAExps _ BFalse = BFalse
mapAExps f (BComp op l r) = BComp op (f l) (f r)
mapAExps f (BNeg b) = BNeg $ mapAExps f b
mapAExps f (BBinExp op l r) = BBinExp op (mapAExps f l) (mapAExps f r)
mapAExps f (BForall i b) = BForall i $ mapAExps f b
mapAExps f (BExists i b) = BExists i $ mapAExps f b
mapAExps f (BPredicate name args) = BPredicate name $ fmap f args

-- Constants

sigma :: State
sigma = Atomic "s"

resultLExp :: LExp FO Refs
resultLExp = LRead sigma $ LIdt $ Idt "\\result"

-- Show instances

instance Show (BExp l m) where
    show BTrue = "true"
    show BFalse = "false"
    show (BComp op l r) = showBinExp op l r
    show (BNeg f) = "!(" ++ show f ++ ")"
    show (BBinExp op l r) = showBinExp op l r
    show (BForall i f) = "forall(" ++ show i ++ ", " ++ show f ++ ")"
    show (BExists i f) = "exists(" ++ show i ++ ", " ++ show f ++ ")"
    show (BPredicate name args) = show name ++ "(" ++ argsList ++ ")"
        where argsList = concat $ intersperse "," $ map show args

showBinExp :: (Show a, Show o) => o -> a -> a -> String
showBinExp op l r = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

instance Show Idt where
    show (Idt s) = s

instance Show (AExp l m) where
    show (ALit i) = show i
    show (ABinExp op l r) = showBinExp op l r
    show (AFunCall name args) = show name ++ "(" ++ argsList ++ ")"
        where argsList = concat $ intersperse "," $ map show args
    show (ALogVar i) = show i
    show (AIdt i) = show i
    show (AAddress l) = "&" ++ show l

instance Show ABinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show (LExp l m) where
    show (LIdt i) = show i
    show (LArray lExp aExp) = show lExp ++ "[" ++ show aExp ++ "]"
    show (LStructurePart lExp idt) = show lExp ++ "." ++ show idt
    show (LRead state lExp) = "read(" ++ show state ++ ", " ++ show lExp ++ ")"
    show (LDeref i) = '*' : show i

instance Show State where
    show (Atomic s) = s
    show (Update state lExp aExp) = "upd(" 
        ++ show state ++ ", " 
        ++ show lExp ++ ", "
        ++ show aExp ++ ")"

instance Show BBinOp where
    show And = "&&"
    show Or = "||"
    show Implies = "->"
    show Iff = "<->"

instance Show CompOp where
    show Less = "<"
    show LessOrEqual = "<=" 
    show Greater = ">" 
    show GreaterOrEqual = ">="
    show Equal = "="
    show NotEqual = "!="

-- Eq and Ord instances

deriving instance Eq (BExp C0 Plain)
deriving instance Eq (BExp C0 Refs)
deriving instance Eq (BExp FO Plain)
deriving instance Eq (BExp FO Refs)

deriving instance Eq (LExp C0 Plain)
deriving instance Eq (LExp C0 Refs)
deriving instance Eq (LExp FO Refs)
deriving instance Eq (LExp FO Plain)

deriving instance Ord (LExp FO Refs)
deriving instance Ord (LExp FO Plain)

deriving instance Eq (AExp C0 Plain)
deriving instance Eq (AExp C0 Refs)
deriving instance Eq (AExp FO Plain)
deriving instance Eq (AExp FO Refs)

deriving instance Ord (AExp FO Refs)
deriving instance Ord (AExp FO Plain)