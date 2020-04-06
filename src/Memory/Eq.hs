{-# LANGUAGE GADTs #-}
module Memory.Eq (MemEq(..), compareLExp) where
import Control.Monad.Reader
import qualified Data.Set as Set
import AST
import Logic.FO
import Logic.Simplified

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
compareDifferentLExp (LRead s1 l1) (LRead s2 l2) = 
    if s1 == s2 then compareLExp l1 l2 else return MemUndecidable
compareDifferentLExp (LRead _ _) _ = return MemUndecidable
compareDifferentLExp _ (LRead _ _) = return MemUndecidable
compareDifferentLExp (LStructurePart struct1 idt1) (LStructurePart struct2 idt2) =
    if idt1 == idt2 then compareLExp struct1 struct2 else return MemNotEq
compareDifferentLExp (LArray arr1 idx1) (LArray arr2 idx2) =
    memAnd <$> compareLExp arr1 arr2 <*> compareAExp idx1 idx2
compareDifferentLExp _ _ = return MemNotEq

compareAExp :: AExpFO -> AExpFO -> Simplified MemEq
compareAExp a b | a == b = return MemEq
compareAExp (ALit _) (ALit _) = return MemNotEq
compareAExp (AIdt l1) (AIdt l2) = do
    comparison <- compareLExp l1 l2
    return $ case comparison of
        MemEq -> MemEq
        _ -> MemUndecidable
compareAExp _ _ = return MemUndecidable

memAnd :: MemEq -> MemEq -> MemEq
memAnd MemEq MemEq = MemEq
memAnd MemNotEq _ = MemNotEq
memAnd _ MemNotEq = MemNotEq
memAnd _ _ = MemUndecidable