{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ImpredicativeTypes #-}

module Expression where

import qualified DeBruijn as DB

import GHC.TypeLits
import HList

data Exp (l :: [*]) (t :: *) where
  Val :: t -> Exp l t
  SVal :: String -> t -> Exp l t
  
  Var :: (KnownSymbol s, HField s l t) => Proxy s -> Exp l t
  Lam :: (KnownSymbol s) => Proxy s -> Exp (Tagged s b ': l) t -> Exp l (b -> t)

  App :: Exp l (t1 -> t2) -> Exp l t1 -> Exp l t2

{- Can't mix de Bruijn and symbol/value
  VarN :: (HNat2Integral n, HLookupByHNat n l) =>
          Proxy n -> Exp l (HLookupByHNatR n l)
  LamN :: Exp (b ': l) t -> Exp l (b -> t)
-}

{- Pretty much subsumed by Lam?
  Pair :: Exp l t1 -> Exp l t2 -> Exp l (t1, t2)
-}

closed :: (forall l. RecordValues l => Exp l t) -> Exp '[] t
closed e = e

instance Show (Exp l t) where
  show (SVal s _) = s
  show (Val _) = "Val"
  show (Var v) = symbolVal v
  show (Lam v b) = "\\" ++ (symbolVal v) ++ " -> " ++ (show b)
  show (App f x) = "(" ++ (show f) ++ " " ++ (show x) ++ ")"
--  show (Pair x y) = "(" ++ (show x) ++ ", " ++ (show y) ++ ")"
  
--  show (LamN b) = "\\" ++ (show b)
--  show (VarN n) = show n

-- call by value evaluator
eval' :: HList l -> Exp l t -> t

eval' _ (SVal _ t) = t
eval' _ (Val t) = t

eval' l (Var v) = hField v l
eval' l (Lam v b) = \x -> eval' (HCons (tagWith v x) l) b

eval' l (App f x) = (eval' l f) (eval' l x)
--eval' l (Pair x y) = (eval' l x, eval' l y)

--eval' l (LamN b) = \x -> eval' (HCons x l) b
--eval' l (VarN v) = hLookupByHNat v l

eval :: Exp '[] t -> t
eval = eval' HNil

{-
whnf' :: HList l -> Exp l t -> Exp '[] t
whnf' l (App f x) =
  case whnf' l f of
    Lam v b -> whnf' (HCons (tagWith v x) l) b
    f'      -> App f' x

whnf' (Val v) = Val v
whnf' (SVal s v) = SVal s v
-}

-- to de Bruijn indices

toDB :: forall l t. Exp l t -> DB.DB (RecordValuesR l) t

toDB (Val t) = DB.Val t
toDB (SVal _ t) = DB.Val t

toDB (App f x) = DB.App (toDB f) (toDB x)
toDB (Lam _ b) = DB.Lam (toDB b)

toDB (Var s) = DB.VarN $ hRecordIndex s (Proxy::Proxy l)

-- helpers
sval :: (Show t) => t -> Exp l t
sval t = SVal (show t) t

app2 f x y = App (App f x) y
app3 f x y z = App (App (App f x) y) z

lift f = App (Val f)
lift2 f = app2 (Val f)
lift3 f = app3 (Val f)

let_ s b e = App (Lam s e) b

-- variables
lx :: Proxy "x"
lx = Proxy
vx = Var lx

ly :: Proxy "y"
ly = Proxy
vy = Var ly

lz :: Proxy "z"
lz = Proxy
vz = Var lz

lf :: Proxy "f"
lf = Proxy
vf = Var lf

#define P(s) (Proxy::Proxy s)
#define V(s) (Var P(s))
#define LAM(s) Lam P(s)
#define LET(s) let_ P(s)

plus = SVal "+" (+)
times = SVal "*" (*)

times2 = LAM("x") $ app2 plus V("x") V("x")

six = eval $ App times2 (Val 3)

-- conditionals

--if_ b x y = if b then x else y
biplex b x y = if b then x else y

-- uses thunks to get around eager evaluation
if_ b x y = App (lift3 biplex b (LAM("_") x) (LAM("_") y)) (Val ())



-- recursion

newtype Rec a = Rec { runRec :: Rec a -> a }

{-
y :: (a -> a) -> a
y f = g (Rec g)
  where g h = f (runRec h h)
        {-# NOINLINE g #-}
-}

-- y-combinator
fixExp = LAM("f") $
           LET("g") (LAM("h") $ App V("f") $ lift2 runRec V("h") V("h"))
                    (App V("g") $ lift Rec V("g"))

factRec = LAM("f") $ LAM("n") $
            if_ (lift2 (==) V("n") (Val 0)) (Val 1)
                (lift2 (*) V("n") $ App V("f") (lift2 (-) V("n") (Val 1)))

fact = App fixExp factRec

fibRec = LAM("f") $ LAM("n") $
            if_ (lift2 (<=) V("n") (Val 1)) V("n")
                (lift2 (+)
                  (App V("f") (lift2 (-) V("n") (Val 2)))
                  (App V("f") (lift2 (-) V("n") (Val 1))))

fib = App fixExp fibRec

-- data structures

--data List a = Nil | Cons (forall t. (a -> List a -> t) -> t)

-- ImpredicativeTypes?
--mk_pair :: Exp l (x -> y -> Pair x y)
mk_pair = LAM("x") $ LAM("y") $ LAM("xy") $ app2 V("xy") V("x") V("y")
get_fst = Lam lz $ App vz $ Lam lx $ Lam ly $ vx
get_snd = Lam lz $ App vz $ Lam lx $ Lam ly $ vy

pair12 = app2 mk_pair (sval 1) (sval 2)
one = eval $ App get_fst pair12
two = eval $ App get_snd pair12

nil = Val Nil

--(<:) :: Exp l a -> Exp l (List a) -> Exp l (List a)
--x <: xs = lift Cons $ app2 mk_pair x xs

