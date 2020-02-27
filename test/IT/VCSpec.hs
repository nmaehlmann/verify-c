module IT.VCSpec where

import Test.Hspec

import AST
import VC

daggerSpec :: IO Spec
daggerSpec = return $ describe "VC.dagger" $ do
    let x = Idt "x"

    it "resolves references" $ do
        let derefX = LDereference $ LIdt x
        let derefedX = LSRead $ ReadLExp sigma $ LSIdt x
        dagger derefX `shouldBe` derefedX

hashmarkSpec :: IO Spec
hashmarkSpec = return $ describe "VC.hashmark" $ do
    let x = Idt "x"

    it "resolves regular variables" $ do
        -- x
        let justX = AIdt $ LIdt x
        let readX = ASRead $ ReadLExp sigma $ LSIdt x
        hashmark justX `shouldBe` readX

    it "resolves references" $ do
        -- *x
        let derefX = AIdt $ LDereference $ LIdt x
        let derefedX = ASRead $ ReadLExp sigma $ LSRead $ ReadLExp sigma $ LSIdt x
        hashmark derefX `shouldBe` derefedX

    it "resolves arrays" $ do
        -- *x[1]
        let derefX = LDereference $ LIdt x
        let array = AIdt $ LArray derefX $ ALit 1
        let derefedX = LSRead $ ReadLExp sigma $ LSIdt x
        let derefedArray = ASRead $ ReadLExp sigma $ LSArray derefedX $ ASLit 1
        hashmark array `shouldBe` derefedArray
