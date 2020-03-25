{-# LANGUAGE GADTs #-}
module ReplaceAExp where
import AST

type ReplaceAExp a = (AExp FO Refs) -> (AExp FO Refs) -> a -> a

bReplaceAExp :: ReplaceAExp (BExp FO Refs)
bReplaceAExp aOld aNew = mapAExps (aReplaceAExp aOld aNew)

aReplaceAExp :: ReplaceAExp (AExp FO Refs)
aReplaceAExp aOld aNew aCurrent | aOld == aCurrent = aNew
aReplaceAExp _ _ (ALit i) = ALit i
aReplaceAExp _ _ (ALogVar v) = ALogVar v
aReplaceAExp aOld aNew (ARead readLExp) = ARead $ rReplaceAExp aOld aNew readLExp
aReplaceAExp aOld aNew (ABinExp op l r) = ABinExp op (aReplaceAExp aOld aNew l) (aReplaceAExp aOld aNew r)
aReplaceAExp aOld aNew (AArray fields) = AArray $ map (aReplaceAExp aOld aNew) fields
aReplaceAExp aOld aNew (AFunCall name args) = AFunCall name $ map (aReplaceAExp aOld aNew) args

rReplaceAExp :: ReplaceAExp (ReadLExp FO)
rReplaceAExp aOld aNew (ReadLExp lNested lSExp) = ReadLExp (sReplaceAExp aOld aNew lNested) (lReplaceAExp aOld aNew lSExp)

sReplaceAExp :: ReplaceAExp State
sReplaceAExp aOld aNew (Update state lExp aExp) = Update
    (sReplaceAExp aOld aNew state) 
    (lReplaceAExp aOld aNew lExp)
    (aReplaceAExp aOld aNew aExp)
sReplaceAExp _ _ s = s

lReplaceAExp :: ReplaceAExp (LExp FO Refs)
lReplaceAExp _ _ (LIdt idt) = LIdt idt
lReplaceAExp aOld aNew (LArray lExp aSExp) = LArray (lReplaceAExp aOld aNew lExp) (aReplaceAExp aOld aNew aSExp)
lReplaceAExp aOld aNew (LStructurePart lExp idt) = LStructurePart (lReplaceAExp aOld aNew lExp) idt
lReplaceAExp aOld aNew (LRead readLExp) = LRead $ rReplaceAExp aOld aNew readLExp