{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE NoMonoLocalBinds #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}

module Church where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad (ap)

import Prelude hiding (foldr, Either(..))

newtype Pair x y = Pair { runPair :: (forall z. (x -> y -> z) -> z) }

pair :: x -> y -> Pair x y
pair x y = Pair $ \p -> p x y

fromPair :: Pair x y -> (x, y)
fromPair (Pair p) = p (,)

mapPair :: (x1 -> x2) -> (y1 -> y2) -> Pair x1 y1 -> Pair x2 y2
mapPair f g (Pair p) = p $ \x1 y1 -> pair (f x1) (g y1)

newtype Either x y = Either { runEither :: (forall z. (x->z) -> (y->z) -> z) }

left :: x -> Either x y
left x = Either $ \l r -> l x

right :: y -> Either x y
right y = Either $ \l r -> r y

--data List a = Nil | Cons (Pair a (List a))

instance Functor (Either l) where
  fmap f (Either lr) = lr left (right . f)

newtype List a = List { runList :: (Either () (Pair a (List a))) }

nil :: List a
nil = List $ left ()

cons :: a -> List a -> List a
cons h t = List . right $ pair h t

instance Functor List where
  fmap f (List l) = List $ fmap (mapPair f (fmap f)) l

instance Foldable List where
  foldr f b (List (Either lr)) =
    lr (const b) (\(Pair p) -> p $ \h t -> f h (foldr f b t))

instance Traversable List where
  sequenceA (List (Either lr)) =
    lr (const $ pure nil)
      (\(Pair p) -> p $ \h t -> cons <$> h <*> sequenceA t)

instance Monoid (List a) where
  mempty = nil
  mappend (List (Either lr)) =
    lr (const id) (\(Pair p) -> p $ \h t -> cons h . (mappend t))

instance Monad List where
  return a = cons a nil
  (>>=) = flip foldMap

instance Applicative List where
  pure = return
  (<*>) = ap

initial = pair (0 :: Int) True

step (Pair p) = p (\i b -> pair (if b then (i + 1) else i) (not b))

pairs = initial : map step pairs

-- tests that the polymorphism in Pair doesn't defeat laziness
-- if it did, then this should take exponential time
test = take 100 $ map fromPair pairs

