module Simplified where
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import FOTypes

type Inequality = Set LExpFO

type Simplified = ReaderT SimplificationCtx Updated

data SimplificationCtx = SimplificationCtx 
    { inequalities :: Set Inequality
    , localVars :: Set LExpFO
    }

data Updated a = Updated a | Unchanged a deriving (Eq, Show)

instance Functor Updated where
    fmap f a = pure f <*> a

instance Applicative Updated where
    pure a = Unchanged a
    (Updated f) <*> a = Updated $ f $ unwrap a
    f <*> (Updated a) = Updated $ unwrap f a
    (Unchanged f) <*> (Unchanged a) = Unchanged $ f a

instance Monad Updated where
    return a = Unchanged a
    (Updated a) >>= f = Updated $ unwrap $ f a
    (Unchanged a) >>= f = f a

unwrap :: Updated a -> a
unwrap (Updated a) = a
unwrap (Unchanged a) = a

notEqual :: LExpFO -> LExpFO -> Inequality
notEqual l r = Set.fromList [l, r]

update :: a -> Simplified a
update a = lift $ Updated a