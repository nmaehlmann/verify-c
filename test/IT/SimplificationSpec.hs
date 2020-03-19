module IT.SimplificationSpec where

import Test.Hspec

import AST
import Simplification
import qualified Data.Set as Set
import Control.Monad.Reader

    
simplificationSpec :: IO Spec
simplificationSpec = return $ describe "Simplification.simplify" $ do
    let x = LSIdt $ Idt "x"
    let y = LSIdt $ Idt "y"

    it "simplifies a read of an update of the same value of plain state" $ do
        let fo = FOComp Equal (ASRead (ReadLExp (Update sigma x (ASLit 6)) x)) (ASLit 0)
        let foSimplified = FOComp Equal (ASLit 6) (ASLit 0)
        simplify fo `shouldBe` foSimplified

    it "simplifies a read of an update of the same value" $ do
        let fo = FOComp Equal (ASRead (ReadLExp (Update (Update sigma y (ASLit 6)) x (ASLit 5)) x)) (ASLit 0)
        let foSimplified = FOComp Equal (ASLit 5) (ASLit 0)
        simplify fo `shouldBe` foSimplified

    it "simplifies unneccessary updates" $ do
        let fo = FOComp Equal (ASRead (ReadLExp (Update sigma y (ASLit 6)) x)) (ASLit 0)
        let foSimplified = FOComp Equal (ASRead (ReadLExp sigma x)) (ASLit 0)
        simplify fo `shouldBe` foSimplified

    it "simplifies a read of an update of the same value with an intermediate value" $ do
        let fo = FOComp Equal (ASRead (ReadLExp (Update (Update sigma x (ASLit 6)) y (ASLit 5)) x)) (ASLit 0)
        let foSimplified = FOComp Equal (ASLit 6) (ASLit 0)
        simplify fo `shouldBe` foSimplified

emptySimplificationCtx = SimplificationCtx { inequalities = Set.empty, localVars = Set.empty}

simplifyASExpSpec :: IO Spec
simplifyASExpSpec = return $ describe "Simplification.simplifyASExp" $ do
    let x = LSIdt $ Idt "x"
    let y = LSIdt $ Idt "y"

    it "simplifies unneccessary updates" $ do
        let aSExp = ASRead (ReadLExp (Update sigma y (ASLit 6)) x)
        let aSExpSimplified = ASRead (ReadLExp sigma x)
        runReaderT (simplifyASExp aSExp) emptySimplificationCtx `shouldBe` (Updated aSExpSimplified)

simplifyReadSpec :: IO Spec
simplifyReadSpec = return $ describe "Simplification.simplifyRead" $ do
    let x = LSIdt $ Idt "x"
    let y = LSIdt $ Idt "y"

    it "simplifies unneccessary updates" $ do
        let readExp = ReadLExp (Update sigma y (ASLit 6)) x
        let aSExpSimplified = ASRead (ReadLExp sigma x)
        runReaderT (simplifyASRead readExp) emptySimplificationCtx `shouldBe` (Updated aSExpSimplified)