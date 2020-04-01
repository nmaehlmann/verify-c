module Memory.LiftSpec where
import Test.Hspec
import AST
import Memory.Lift

spec :: Spec
spec = do
    describe "dagger" $ do
        let x = Idt "x"

        it "resolves references" $ do
            let derefX = LDeref $ LIdt x
            let derefedX = LRead $ ReadLExp sigma $ LIdt x
            dagger derefX `shouldBe` derefedX
        
        it "resolves references to references" $ do
            let doublyDerefX = LDeref $ LDeref $ LIdt x
            let doublyDerefedX = LRead $ ReadLExp sigma $ LRead $ ReadLExp sigma $ LIdt x
            dagger doublyDerefX `shouldBe` doublyDerefedX

    describe "hashmark" $ do
        let x = Idt "x"

        it "resolves regular variables" $ do
            -- x
            let justX = AIdt $ LIdt x
            let readX = ARead $ ReadLExp sigma $ LIdt x
            hashmark justX `shouldBe` readX

        it "resolves references" $ do
            -- *x
            let derefX = AIdt $ LDeref $ LIdt x
            let derefedX = ARead $ ReadLExp sigma $ LRead $ ReadLExp sigma $ LIdt x
            hashmark derefX `shouldBe` derefedX

        it "resolves arrays" $ do
            -- *x[1]
            let derefX = LDeref $ LIdt x
            let array = AIdt $ LArray derefX $ ALit 1
            let derefedX = LRead $ ReadLExp sigma $ LIdt x
            let derefedArray = ARead $ ReadLExp sigma $ LArray derefedX $ ALit 1
            hashmark array `shouldBe` derefedArray

        it "resolves addition of two arrays" $ do
            let derefX = LDeref $ LIdt x
            let array = AIdt $ LArray derefX $ ALit 1
            let derefedX = LRead $ ReadLExp sigma $ LIdt x
            let derefedArray = ARead $ ReadLExp sigma $ LArray derefedX $ ALit 1
            let addition = ABinExp Add array array
            let derefedAddition = ABinExp Add derefedArray derefedArray
            hashmark addition `shouldBe` derefedAddition
