module Logic.SimplifySpec where

import Test.Hspec

import AST
import Logic.Simplify
import Logic.Simplified
import qualified Data.Set as Set
import Control.Monad.Reader

    
spec :: Spec
spec = do
    describe "simplify" $ do
        let x = LIdt $ Idt "x"
        let y = LIdt $ Idt "y"

        it "simplifies a read of an update of the same value of plain state" $ do
            let fo = BComp Equal (AIdt (LRead (Update sigma x (ALit 6)) x)) (ALit 0)
            let foSimplified = BComp Equal (ALit 6) (ALit 0)
            simplify fo `shouldBe` foSimplified

        it "simplifies a read of an update of the same value" $ do
            let fo = BComp Equal (AIdt (LRead (Update (Update sigma y (ALit 6)) x (ALit 5)) x)) (ALit 0)
            let foSimplified = BComp Equal (ALit 5) (ALit 0)
            simplify fo `shouldBe` foSimplified

        it "simplifies unneccessary updates" $ do
            let fo = BComp Equal (AIdt (LRead (Update sigma y (ALit 6)) x)) (ALit 0)
            let foSimplified = BComp Equal (AIdt (LRead sigma x)) (ALit 0)
            simplify fo `shouldBe` foSimplified

        it "simplifies a read of an update of the same value with an intermediate value" $ do
            let fo = BComp Equal (AIdt (LRead (Update (Update sigma x (ALit 6)) y (ALit 5)) x)) (ALit 0)
            let foSimplified = BComp Equal (ALit 6) (ALit 0)
            simplify fo `shouldBe` foSimplified

    describe "simplifyAExp" $ do
        let x = LIdt $ Idt "x"
        let y = LIdt $ Idt "y"

        it "simplifies unneccessary updates" $ do
            let aExp = AIdt (LRead (Update sigma y (ALit 6)) x)
            let aExpSimplified = AIdt (LRead sigma x)
            runReaderT (simplifyAExp aExp) emptySimplificationCtx `shouldBe` (Updated aExpSimplified)

emptySimplificationCtx = SimplificationCtx { inequalities = Set.empty, localVars = Set.empty}