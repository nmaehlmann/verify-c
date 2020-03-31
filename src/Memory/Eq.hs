{-# LANGUAGE GADTs #-}
module Memory.Eq (MemEq(..), compareLExp) where
import Control.Monad.Reader
import qualified Data.Set as Set
import AST
import Logic.FO
import Simplified

data MemEq = MemEq | MemNotEq |MemUndecidable

compareLExp :: LExpFO -> LExpFO -> Simplified MemEq
compareLExp a b | (a == b) = return MemEq
compareLExp a b = do
    ineqs <- inequalities <$> ask
    decls <- localVars <$> ask
    let predefindedNEq = Set.member (notEqual a b) ineqs
    let anyIsFresh = Set.member a decls || Set.member b decls
    if predefindedNEq || anyIsFresh 
        then return MemNotEq 
        else compareDifferentLExp a b

compareDifferentLExp :: LExpFO -> LExpFO -> Simplified MemEq
compareDifferentLExp (LRead _) _ = return MemUndecidable
compareDifferentLExp _ (LRead _) = return MemUndecidable
compareDifferentLExp (LStructurePart struct1 idt1) (LStructurePart struct2 idt2) =
    if idt1 == idt2 then compareLExp struct1 struct2 else return MemNotEq
compareDifferentLExp (LArray arr1 idx1) (LArray arr2 idx2) = do
    eqArray <- compareLExp arr1 arr2
    eqIdx <- compareAExp idx1 idx2
    return $ eqArray `memAnd` eqIdx
compareDifferentLExp _ _ = return MemNotEq

compareAExp :: AExpFO -> AExpFO -> Simplified MemEq
compareAExp a b | a == b = return MemEq
compareAExp (ALit _) (ALit _) = return MemNotEq
compareAExp (ARead (ReadLExp s1 l1)) (ARead (ReadLExp s2 l2)) | s1 == s2 = do
    ineqs <- inequalities <$> ask
    let predefindedNEq = Set.member (notEqual l1 l2) ineqs
    sameLExp <- compareLExp l1 l2
    return $ case sameLExp of
        MemEq -> MemEq
        _ -> if predefindedNEq then MemNotEq else MemUndecidable
compareAExp _ _ = return MemUndecidable

memAnd :: MemEq -> MemEq -> MemEq
memAnd MemUndecidable _ = MemUndecidable
memAnd _ MemUndecidable = MemUndecidable
memAnd MemEq MemEq = MemEq
memAnd _ _ = MemNotEq