{-# LANGUAGE GADTs #-}
module Replace.State (replaceState) where
import AST

type ReplaceState a = State -> State -> a -> a

replaceState :: ReplaceState (BExp FO Refs)
replaceState sOld sNew = mapAExps (aReplaceState sOld sNew)

aReplaceState :: ReplaceState (AExp FO Refs)
aReplaceState _ _ (ALit i) = ALit i
aReplaceState _ _ (ALogVar v) = ALogVar v
aReplaceState sOld sNew (ABinExp op l r) = ABinExp op (aReplaceState sOld sNew l) (aReplaceState sOld sNew r)
aReplaceState sOld sNew (AFunCall name args) = AFunCall name $ map (aReplaceState sOld sNew) args
aReplaceState sOld sNew (AIdt l) = AIdt $ lReplaceState sOld sNew l

rReplaceState :: ReplaceState (ReadLExp FO)
rReplaceState sOld sNew (ReadLExp sNested lSExp) = ReadLExp (sReplaceState sOld sNew sNested) (lReplaceState sOld sNew lSExp)

sReplaceState :: ReplaceState State
sReplaceState sOld sNew sNested | sOld == sNested = sNew
sReplaceState sOld sNew (Update state lExp aExp) = Update 
    (sReplaceState sOld sNew state) 
    (lReplaceState sOld sNew lExp)
    (aReplaceState sOld sNew aExp)
sReplaceState _ _ s = s

lReplaceState :: ReplaceState (LExp FO Refs)
lReplaceState _ _ (LIdt idt) = LIdt idt
lReplaceState sOld sNew (LArray lExp aSExp) = LArray (lReplaceState sOld sNew lExp) (aReplaceState sOld sNew aSExp)
lReplaceState sOld sNew (LStructurePart lExp idt) = LStructurePart (lReplaceState sOld sNew lExp) idt
lReplaceState sOld sNew (LRead readLExp) = LRead $ rReplaceState sOld sNew readLExp