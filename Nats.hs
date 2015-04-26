{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- This is all probably in base now?
module Nats where

import Data.HList

import qualified GHC.TypeLits as TL

data Nat = Z | S Nat

type family Min (x :: Nat) (y :: Nat) :: Nat where
  Min Z x = Z
  Min x Z = Z
  Min (S x) (S y) = S (Min x y)

type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max Z x = x
  Max x Z = S x
  Max (S x) (S y) = S (Max x y)

infixl 6 :+:
type family (x :: Nat) :+: (y :: Nat) :: Nat where
  Z :+: x = x
  (S x) :+: y = S (x :+: y)

infixl 7 :*:
type family (x :: Nat) :*: (y :: Nat) :: Nat where
  Z :*: x = Z
  (S x) :*: y = y :+: (x :*: y)

-- use singletons?

type family ToNat (n :: TL.Nat) :: Nat where
  ToNat 0 = Z
  ToNat n = S (ToNat (n TL.- 1))

type family ToHNat (n :: TL.Nat) :: HNat where
  ToHNat 0 = HZero
  ToHNat n = HSucc (ToHNat (n TL.- 1))

data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

class HasNat (n :: Nat) where
  nat :: SNat n

instance HasNat Z where
  nat = SZ

instance (HasNat n) => HasNat (S n) where
  nat = SS nat

snat :: (n' ~ ToNat n, HasNat n') => proxy n -> SNat n'
snat _ = nat

zero = SZ
one = SS zero
two = SS one
three = SS two

nPred :: proxy (S n) -> Proxy n
nPred _ = Proxy

nSucc :: proxy n -> Proxy (S n)
nSucc _ = Proxy

