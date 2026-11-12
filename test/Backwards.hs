{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Backwards where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, testProperty, Arbitrary (..), Arbitrary1, Property, Testable (..), CoArbitrary)
import Test.QuickCheck.Classes.Base (monadLaws, Laws (..))
import Control.Applicative.Backwards (Backwards(..))
import Data.Proxy
import Data.List.NonEmpty (NonEmpty)
import Data.Functor.Identity (Identity)
import Data.Tuple.Solo (Solo)
import Data.Complex (Complex)
import Control.Applicative (ZipList(ZipList))
import Data.Functor.Product (Product)
import Data.Functor.Compose (Compose)
import Control.Monad (ap, liftM2)
import Control.Monad.Trans.Reader (Reader, ReaderT (ReaderT), runReader)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import Text.Show.Functions ()
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import Control.Applicative

deriving newtype instance Arbitrary (m a) => Arbitrary (Backwards m a)
deriving newtype instance (Arbitrary (m (a, w))) => Arbitrary (Lazy.WriterT w m a)
deriving newtype instance (Arbitrary (m (a, w))) => Arbitrary (Strict.WriterT w m a)
deriving newtype instance (CoArbitrary r, Arbitrary (m a)) => Arbitrary (ReaderT r m a)
deriving newtype instance Show (ReaderT r m a)
deriving newtype instance (CoArbitrary s, Arbitrary (m (a, s))) => Arbitrary (Lazy.StateT s m a)
deriving newtype instance Show (Lazy.StateT s m a)
deriving newtype instance (CoArbitrary s, Arbitrary (m (a, s))) => Arbitrary (Strict.StateT s m a)
deriving newtype instance Show (Strict.StateT s m a)


newtype Apply f a = Apply { getApply :: f a }

deriving instance (forall x. Eq x => Eq (f x), Eq a) => Eq (Apply f a)
deriving instance (forall x. Arbitrary x => Arbitrary (f x), Arbitrary a) => Arbitrary (Apply f a)
deriving instance (forall x. Show x => Show (f x), Show a) => Show (Apply f a)

-- this is a quadratic equation
data QuadraticEquation = QuadraticEquation
  { _quadraticEquationQuadratic :: Integer
  , _quadraticEquationLinear :: Integer
  , _quadraticEquationConstant :: Integer
  }
  deriving (Eq)

-- This show instance is does not actually provide a
-- way to create an equation. Instead, it makes it look
-- like a lambda.
instance Show QuadraticEquation where
  show (QuadraticEquation a b c) = "\\x -> " ++ show a ++ " * x ^ 2 + " ++ show b ++ " * x + " ++ show c

instance Arbitrary QuadraticEquation where
  arbitrary = do
    (a,b,c) <- arbitrary
    return (QuadraticEquation (abs a) (abs b) (abs c))
  shrink (QuadraticEquation a b c) =
    let xs = shrink (a,b,c)
     in map (\(x,y,z) -> QuadraticEquation (abs x) (abs y) (abs z)) xs

runQuadraticEquation :: QuadraticEquation -> Integer -> Integer
runQuadraticEquation (QuadraticEquation a b c) x = a * x ^ (2 :: Integer) + b * x + c


monadAp :: forall proxy f.
  (Monad f, forall a. Eq a => Eq (f a), forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a))
  => proxy f -> Property
monadAp _ = property $ \(Apply (f :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  (liftM2 runQuadraticEquation f x) == (liftA2 runQuadraticEquation f x)


monadApWith :: forall proxy f prop.
  (Monad f, forall a. Show a => Show (f a), forall a. Arbitrary a => Arbitrary (f a), Testable prop)
  => proxy f -> (forall a . Eq a => f a -> f a -> prop) -> Property
monadApWith _ eq = property $ \(Apply (f :: f QuadraticEquation)) (Apply (x :: f Integer)) ->
  eq (liftM2 runQuadraticEquation f x) (liftA2 runQuadraticEquation f x)

test :: TestTree
test = testGroup "Backwards Monad"
  [ testProperty "Identity" $ monadAp (Proxy :: Proxy (Backwards Identity))
  , testProperty "Solo" $ monadAp (Proxy :: Proxy (Backwards Solo))
  , testProperty "Maybe" $ monadAp (Proxy :: Proxy (Backwards Maybe))
  , testProperty "List" $ monadAp (Proxy :: Proxy (Backwards []))
  , testProperty "NonEmpty" $ monadAp (Proxy :: Proxy (Backwards NonEmpty))
  , testProperty "Either" $ monadAp (Proxy :: Proxy (Backwards (Either Integer)))
  , testProperty "Product" $ monadAp (Proxy :: Proxy (Backwards (Product NonEmpty Maybe)))
  , testProperty "Lazy Writer" $ monadAp (Proxy :: Proxy (Backwards (Lazy.Writer [Integer])))
  , testProperty "Strict Writer" $ monadAp (Proxy :: Proxy (Backwards (Strict.Writer [Integer])))
  , testProperty "Reader" $ monadApWith (Proxy :: Proxy (Backwards (Reader Integer))) $ \ (Backwards f1) (Backwards f2) r -> runReader f1 r == runReader f2 r
  , testProperty "Lazt State" $ monadApWith (Proxy :: Proxy (Backwards (Lazy.State Integer))) $ \ (Backwards f1) (Backwards f2) s -> Lazy.runState f1 s == Lazy.runState f2 s
  , testProperty "Strict State" $ monadApWith (Proxy :: Proxy (Backwards (Strict.State Integer))) $ \ (Backwards f1) (Backwards f2) s -> Strict.runState f1 s == Strict.runState f2 s
  ]
