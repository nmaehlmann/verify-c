{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AST where
import Data.List

data State
    = Atomic String
    | Update State (LExpr FO Refs) (AExpr FO Refs)
    deriving (Eq, Ord)

deriving instance Eq (LExpr FO Refs)
deriving instance Eq (LExpr FO Plain)
deriving instance Ord (LExpr FO Refs)
deriving instance Ord (LExpr FO Plain)
deriving instance Eq (AExpr FO Refs)
deriving instance Eq (AExpr FO Plain)
deriving instance Ord (AExpr FO Refs)
deriving instance Ord (AExpr FO Plain)
deriving instance Eq (ReadLExpr FO)
deriving instance Ord (ReadLExpr FO)

sigma :: State
sigma = Atomic "s"

data ABinOp = Add | Sub | Mul | Div
    deriving (Eq, Ord)

data CompOp = Less | LessOrEqual | Greater | GreaterOrEqual | Equal | NotEqual
    deriving (Eq)

data BBinOp = And | Or | Implies
    deriving (Eq)

type LExpr' l = LExpr l Plain
type AExpr' l = AExpr l Plain

type LExpr'' = LExpr' C0
type AExpr'' = AExpr' C0

type BExpr' l = BExpr l Plain

data Stmt 
    = Assignment LExpr'' AExpr''
    | ITE (BExpr' C0) Stmt Stmt
    | While (BExpr' C0) (BExpr' FO) Stmt
    | Seq Stmt Stmt
    | Return (Maybe AExpr'')
    | Assertion (BExpr'  FO)
    | Declaration LExpr''
    | FunCall (Maybe LExpr'') Idt [AExpr'']
    | Empty

data Program = Program [FunctionDefinition]

data FunctionDefinition = FunctionDefinition 
    { funDefType      :: Type
    , funDefName      :: Idt
    , funDefArgs      :: [Decl]
    , funDefPrecond   :: BExpr' FO
    , funDefPostcond  :: BExpr' FO
    , funDefBody      :: Stmt
    }

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

data C0 = C0
data FO = FO
data Refs = Refs
data Plain = Plain

data BExpr l m where
    BTrue       :: BExpr l m
    BFalse      :: BExpr l m
    BNeg        :: BExpr l m -> BExpr l m
    BBinExp     :: BBinOp -> BExpr l m -> BExpr l m -> BExpr l m
    BComp       :: CompOp -> AExpr l m -> AExpr l m -> BExpr l m
    BForall     :: Idt -> BExpr FO m -> BExpr FO m
    BExists     :: Idt -> BExpr FO m -> BExpr FO m
    BPredicate  :: Idt -> [AExpr FO m] -> BExpr FO m

data LExpr l m where
    LIdt            :: Idt -> LExpr l m
    LArray          :: LExpr l m -> AExpr l m -> LExpr l m
    LStructurePart  :: LExpr l m -> Idt -> LExpr l m
    LRead           :: ReadLExpr l -> LExpr l Refs
    LDeref          :: LExpr l Plain -> LExpr l Plain

data AExpr l m where
    ALit        :: Integer -> AExpr l m
    AIdt        :: LExpr l Plain -> AExpr l Plain
    ARead       :: ReadLExpr FO -> AExpr FO Refs
    ABinExp     :: ABinOp -> AExpr l m -> AExpr l m -> AExpr l m
    AArray      :: [AExpr l m] -> AExpr l m
    AFunCall    :: Idt -> [AExpr FO m] -> AExpr FO m
    ALogVar     :: Idt -> AExpr FO m

data ReadLExpr l = ReadLExpr State (LExpr l Refs)

mapAExps :: (AExpr l m1 -> AExpr FO m2) -> (BExpr l m1) -> (BExpr FO m2)
mapAExps _ BTrue = BTrue
mapAExps _ BFalse = BFalse
mapAExps f (BComp op l r) = BComp op (f l) (f r)
mapAExps f (BNeg b) = BNeg $ mapAExps f b
mapAExps f (BBinExp op l r) = BBinExp op (mapAExps f l) (mapAExps f r)
mapAExps f (BForall i b) = BForall i $ mapAExps f b
mapAExps f (BExists i b) = BExists i $ mapAExps f b
mapAExps f (BPredicate name args) = BPredicate name $ fmap f args

-- Show instances

instance Show (BExpr l m) where
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

instance Show (AExpr l m) where
    show (ALit i) = show i
    show (ARead readLExp) = show readLExp
    show (ABinExp op l r) = showBinExp op l r
    show (AArray fields) = "[" ++ showFields ++ "]" 
        where showFields = concat $ intersperse "," $ map show fields
    show (AFunCall name args) = show name ++ "(" ++ argsList ++ ")"
        where argsList = concat $ intersperse "," $ map show args
    show (ALogVar i) = show i
    show (AIdt i) = show i

instance Show ABinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show (ReadLExpr l) where
    -- show (ReadLExp (Atomic _) (LSIdt i)) = show i
    show (ReadLExpr state lSExp) = "read(" ++ show state ++ ", " ++ show lSExp ++ ")"

instance Show (LExpr l m) where
    show (LIdt i) = show i
    show (LArray lSExp aSExp) = show lSExp ++ "[" ++ show aSExp ++ "]"
    show (LStructurePart lSExp idt) = show lSExp ++ "." ++ show idt
    show (LRead r) = show r
    show (LDeref i) = '*' : show i

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