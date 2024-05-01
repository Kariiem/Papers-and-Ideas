-- | Church Encoding of Data Types Considered Harmful for Implementations
{-# LANGUAGE NoImplicitPrelude #-}


module Main where

import Prelude hiding (Num, succ, pred)

main = return ()

class Pair t where
  pair :: a -> b -> t a b
  e1 :: t a b -> a
  e2 :: t a b -> b

swap :: (Pair t) => t a b -> t b a
swap p = pair (e2 p) (e1 p)

instance Pair (,) where
  pair a b = (a, b)
  e1 (a, b) = a
  e2 (a, b) = b

newtype FPair a b = FPair (forall r. (a -> b -> r) -> r)

instance Pair FPair where
  pair a b = FPair $ \f -> f a b
  e1 (FPair p) = p $ \a b -> a
  e2 (FPair p) = p $ \a b -> b


class Num t where
  zero   :: t
  succ   :: t -> t
  isZero :: t -> Bool
  pred   :: t -> t

apply :: (a -> a) -> Int -> a -> a 
apply f 0 z = z
apply f n z = apply f (n - 1) (f z)

toInt :: (Num t) => t -> Int
toInt n = if isZero n then 0 else 1 + (toInt $ pred n)
-- -------------------------------------------------------
-- -------------------------------------------------------
instance Num Int where
  zero     = 0
  succ   n = n + 1
  isZero n = n == 0
  pred   n = if n > 0 then n - 1 else undefined
mkInt n = n 
-- --------------------------------------------------------
-- Algebraic Data Type
data Peano = Z | S Peano deriving (Eq, Show)
instance Num Peano where
  zero     = Z
  succ     = S
  isZero n = case n of Z -> True      ; _   -> False
  pred   n = case n of Z -> undefined ; S m -> m

mkPeano :: Int -> Peano
mkPeano n =  apply S n Z
-- --------------------------------------------------------
-- Church Encoding
newtype CNum = CNum (forall r. r -> (r -> r) -> r)
instance Show CNum where
  show (CNum n) = n "zero" (\s -> "Succ (" ++ s ++ ")")
instance Num CNum where
  zero            = CNum $ \z s -> z
  succ   (CNum n) = CNum $ \z s -> s (n z s)
  isZero (CNum n) = n True (\_ -> False)
  pred   (CNum n) = CNum $ \z s ->
                             e1 (n
                                  (undefined `fpair` z)
                                  (\p -> (e2 p) `fpair` (s (e2 p))))
    where
      fpair = (pair :: a -> b -> FPair a b)
mkCNum :: Int -> CNum
mkCNum n = CNum $ \z s -> apply s n z
-- --------------------------------------------------------
-- Scott Encoding
newtype SNum = SNum (forall r. r -> (SNum -> r) -> r)
instance Show SNum where
  show (SNum n) = n "zero" (\s -> "Succ (" ++ show s ++ ")")
instance Num SNum where
  zero            = SNum $ \z s -> z
  succ   n        = SNum $ \z s -> s n
  isZero (SNum n) = n True (\x -> False)
  pred   (SNum n) = n undefined id

mkSNum :: Int -> SNum
mkSNum 0 = SNum $ \z s -> z
mkSNum n = SNum $ \z s -> s . mkSNum $ n - 1
-- --------------------------------------------------------
-- Parigot Encoding
newtype PNum = PNum (forall r. r -> (PNum -> r -> r) -> r)
instance Show PNum where
  show (PNum n) = n "zero" (\s c -> "Succ (" ++ show s ++ ")")
instance Num PNum where
  zero            = PNum $ \z s -> z
  succ p@(PNum n) = PNum $ \z s -> s p (n z s)
  isZero (PNum n) = n True (\p x -> False)
  pred   (PNum n) = n undefined (\p x -> p)

mkPNum :: Int -> PNum
mkPNum 0 = PNum $ \z s -> z
mkPNum n = PNum $ \z s -> let p@(PNum f) = mkPNum (n - 1)
                              in s p (f z s)

inf :: Num t => t
inf = succ inf

