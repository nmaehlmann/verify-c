import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
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
        let isValid = isRight

        it "parses boolean constants" $ do
            parseBExp "true" `shouldBe` Right BTrue
            parseBExp "false" `shouldBe` Right BFalse
            parseBExp "maybe" `shouldNotSatisfy` isValid

        it "parses comparisons" $ do
            parseBExp "x < y" `shouldBe` (Right (BLess x y))
            parseBExp "x = y" `shouldBe` (Right (BEq x y))

        it "ignores whitespaces in '<' comparisons" $ do
            whitespaceIndependent ["x","<","y"] (\s -> parseBExp s `shouldBe` (Right (BLess x y)))
        
        it "ignores whitespaces in '=' comparisons" $ do
            whitespaceIndependent ["x","=","y"] $ \s -> parseBExp s `shouldBe` (Right (BEq x y))