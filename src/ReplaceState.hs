{-# LANGUAGE GADTs #-}
module ReplaceState where
import AST

type ReplaceState a = State -> State -> a -> a

bReplaceState :: ReplaceState (BExpr FO Refs)
bReplaceState sOld sNew = mapAExps (aReplaceState sOld sNew)

aReplaceState :: ReplaceState (AExpr FO Refs)
aReplaceState _ _ (ALit i) = ALit i
aReplaceState _ _ (ALogVar v) = ALogVar v
aReplaceState sOld sNew (ARead readLExp) = ARead $ rReplaceState sOld sNew readLExp
aReplaceState sOld sNew (ABinExp op l r) = ABinExp op (aReplaceState sOld sNew l) (aReplaceState sOld sNew r)
aReplaceState sOld sNew (AArray fields) = AArray $ map (aReplaceState sOld sNew) fields
aReplaceState sOld sNew (AFunCall name args) = AFunCall name $ map (aReplaceState sOld sNew) args

rReplaceState :: ReplaceState (ReadLExpr FO)
rReplaceState sOld sNew (ReadLExpr sNested lSExp) = ReadLExpr (sReplaceState sOld sNew sNested) (lReplaceState sOld sNew lSExp)

sReplaceState :: ReplaceState State
sReplaceState sOld sNew sNested | sOld == sNested = sNew
sReplaceState sOld sNew (Update state lSExp aSExp) = Update 
    (sReplaceState sOld sNew state) 
    (lReplaceState sOld sNew lSExp)
    (aReplaceState sOld sNew aSExp)
sReplaceState _ _ s = s

lReplaceState :: ReplaceState (LExpr FO Refs)
lReplaceState _ _ (LIdt idt) = LIdt idt
lReplaceState sOld sNew (LArray lSExp aSExp) = LArray (lReplaceState sOld sNew lSExp) (aReplaceState sOld sNew aSExp)
lReplaceState sOld sNew (LStructurePart lSExp idt) = LStructurePart (lReplaceState sOld sNew lSExp) idt
lReplaceState sOld sNew (LRead readLExp) = LRead $ rReplaceState sOld sNew readLExp