{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE NoMonoLocalBinds #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Pair where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative

import Prelude hiding (foldr)

newtype Pair x y = Pair (forall z. (x -> y -> z) -> z)

pair :: x -> y -> Pair x y
pair x y = Pair $ \p -> p x y

fromPair :: Pair x y -> (x, y)
fromPair (Pair p) = p (,)

data List a = Nil | Cons (Pair a (List a))

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons (Pair p)) = p (\x xs -> Cons $ pair (f x) (fmap f xs))

instance Foldable List where
  foldr _ b Nil = b
  foldr f b (Cons (Pair p)) = p (\x xs -> f x (foldr f b xs))

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons (Pair p)) =
--    p (\x xs -> Cons <$> (_ <$> x <*> sequenceA xs))
    p (\x xs -> Cons <$> (pair <$> x <*> sequenceA xs))

initial = pair (0 :: Int) True

step (Pair p) = p (\i b -> pair (if b then (i + 1) else i) (not b))

pairs = initial : map step pairs

-- tests that the polymorphism in Pair doesn't defeat laziness
-- if it did, then this should take exponential time
test = take 100 $ map fromPair pairs

