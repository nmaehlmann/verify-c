import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Data.Either

import AST
import Parser.BooleanExpression

insertWhiteSpaces :: [String] -> Gen String
insertWhiteSpaces [] = spacesGen
insertWhiteSpaces (x:xs) = do
    s <- spacesGen
    next <- insertWhiteSpaces xs
    return $ s ++ x ++ next

spacesGen :: Gen String
spacesGen = do
    numOfSpaces <- (arbitrary :: Gen Int) `suchThat` (>= 0)
    return $ replicate numOfSpaces ' '

whitespaceIndependent s f = property $ forAll (insertWhiteSpaces s) $ f

main :: IO ()
main = hspec $ do
    describe "Parser.BooleanExpression" $ do
        let parseBExp t = parse bExp "" t
        let x = AIdt $ Idt "x"
        let y = AIdt $ Idt "y"
        let z = AIdt $ Idt "z"
        let isValid = isRight

        it "parses boolean constants" $ do
            parseBExp "true" `shouldBe` Right BTrue
            parseBExp "false" `shouldBe` Right BFalse
            parseBExp "maybe" `shouldNotSatisfy` isValid

        it "parses simple comparisons" $ do
            parseBExp "x < y" `shouldBe` (Right (BLess x y))
            parseBExp "x = y" `shouldBe` (Right (BEq x y))

        it "ignores whitespaces in '<' comparisons" $ do
            whitespaceIndependent ["x","<","y"] $ \s -> parseBExp s `shouldBe` (Right (BLess x y))
        
        it "ignores whitespaces in '=' comparisons" $ do
            whitespaceIndependent ["x","=","y"] $ \s -> parseBExp s `shouldBe` (Right (BEq x y))

        it "parses simple conjunctions" $ do
            parseBExp "x < y && y < z" `shouldBe` (Right (BAnd (BLess x y) (BLess y z)))

        it "parses simple disjunctions" $ do
            parseBExp "x < y || y < z" `shouldBe` (Right (BOr (BLess x y) (BLess y z)))

        it "ignores whitespaces in conjunctions" $ do
            whitespaceIndependent ["x < y", "&&", "y < z"] $ 
                \s -> parseBExp s `shouldBe` (Right (BAnd (BLess x y) (BLess y z)))
    
        it "ignores whitespaces in disjunctions" $ do
            whitespaceIndependent ["x < y", "||", "y < z"] $ 
                \s -> parseBExp s `shouldBe` (Right (BOr (BLess x y) (BLess y z)))
