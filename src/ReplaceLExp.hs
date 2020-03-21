{-# LANGUAGE GADTs #-}
module ReplaceLExp where
import AST

type ReplaceLExp a = (LExp FO Refs) -> (LExp FO Refs) -> a -> a

bReplaceLExp :: ReplaceLExp (BExp FO Refs)
bReplaceLExp lOld lNew = mapAExps (aReplaceLExp lOld lNew)

aReplaceLExp :: ReplaceLExp (AExp FO Refs)
aReplaceLExp _ _ (ALit i) = ALit i
aReplaceLExp _ _ (ALogVar v) = ALogVar v
aReplaceLExp lOld lNew (ARead readLExp) = ARead $ rReplaceLExp lOld lNew readLExp
aReplaceLExp lOld lNew (ABinExp op l r) = ABinExp op (aReplaceLExp lOld lNew l) (aReplaceLExp lOld lNew r)
aReplaceLExp lOld lNew (AArray fields) = AArray $ map (aReplaceLExp lOld lNew) fields
aReplaceLExp lOld lNew (AFunCall name args) = AFunCall name $ map (aReplaceLExp lOld lNew) args

rReplaceLExp :: ReplaceLExp (ReadLExp FO)
rReplaceLExp lOld lNew (ReadLExp lNested lSExp) = ReadLExp (sReplaceLExp lOld lNew lNested) (lReplaceLExp lOld lNew lSExp)

sReplaceLExp :: ReplaceLExp State
sReplaceLExp lOld lNew (Update state lExp aExp) = Update
    (sReplaceLExp lOld lNew state) 
    (lReplaceLExp lOld lNew lExp)
    (aReplaceLExp lOld lNew aExp)
sReplaceLExp _ _ s = s

lReplaceLExp :: ReplaceLExp (LExp FO Refs)
lReplaceLExp lOld lNew lNested | lOld == lNested = lNew
lReplaceLExp _ _ (LIdt idt) = LIdt idt
lReplaceLExp lOld lNew (LArray lExp aSExp) = LArray (lReplaceLExp lOld lNew lExp) (aReplaceLExp lOld lNew aSExp)
lReplaceLExp lOld lNew (LStructurePart lExp idt) = LStructurePart (lReplaceLExp lOld lNew lExp) idt
lReplaceLExp lOld lNew (LRead readLExp) = LRead $ rReplaceLExp lOld lNew readLExp