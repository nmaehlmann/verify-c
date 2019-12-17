module IT.Whitespaces (whitespaceIndependent) where

import Test.Hspec
import Test.QuickCheck

insertWhiteSpaces :: [String] -> Gen String
insertWhiteSpaces [] = return ""
insertWhiteSpaces (x:xs) = (\spacedXs -> x ++ spacedXs) <$> insertWhiteSpaces' xs
insertWhiteSpaces' [] = spacesGen
insertWhiteSpaces' (x:xs) = do
    s <- spacesGen
    next <- insertWhiteSpaces xs
    return $ s ++ x ++ next

spacesGen :: Gen String
spacesGen = do
    numOfSpaces <- (arbitrary :: Gen Int) `suchThat` (>= 0)
    return $ replicate numOfSpaces ' '

whitespaceIndependent s f = property $ forAll (insertWhiteSpaces s) $ f
