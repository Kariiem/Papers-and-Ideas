-- | Codewars: Coroutines
-- | link: https://www.codewars.com/kata/547a77a6b84a1fb8bf000211/train/haskell

{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Coroutine where

import Control.Monad (ap, forever)

-- newtype T = T a
-- newtype T = T (forall r. (a -> r) -> r)
-- newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r }
--  deriving (Functor)

data Coroutine u d a = Done a
                       | Out d (Coroutine u d a)
                       | In (u -> Coroutine u d a) deriving Functor

instance (Show u, Show d, Show a) => Show (Coroutine u d a) where
  show (Done a) = "(Done " ++ show a ++ ")"
  show (Out d c) = show d ++ " |> " ++ show c
  show (In h) = "Input"

instance Applicative (Coroutine u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine u d) where
  return x = Done x
  f >>= g  = case f of
               Done a  -> g a
               In h    -> In (\u -> h u >>= g)
               Out d c -> Out d (c >>= g)

(>>>) :: Coroutine i m a -> Coroutine m o a -> Coroutine i o a
x >>> y = connectL y x
  where
    connectL :: Coroutine m o a -> Coroutine i m a -> Coroutine i o a
    connectL (Done a)  l = Done a
    connectL (Out d c) l = Out d (connectL c l)
    connectL (In h)    l = connectR l h

    connectR :: Coroutine i m a -> (m -> Coroutine m o a) -> Coroutine i o a
    connectR (Done a)  r = Done a
    connectR (Out d c) r = connectL (r d) c
    connectR (In f)    r = In (\u -> connectR (f u) r) 

(>|>) :: Coroutine i m a -> Coroutine m o a -> Coroutine i o a
x >|> y = case y of
            Done a  -> Done a
            Out d c -> Out d (x >|> c)
            In h    -> pipe2 x h

-- It might be useful to define the following function
pipe2 :: Coroutine u m a -> (m -> Coroutine m d a) -> Coroutine u d a
pipe2 x h =  case x of
               Done a  -> Done a
               Out d c -> c >>> h d
               In f    -> In (\u -> pipe2 (f u) h)

-- Library functions

output :: a -> Coroutine u a ()
output v = Out v (Done ())

input :: Coroutine v d v
input = In (\u -> Done u)

produce :: [a] -> Coroutine u a ()
produce [] = return ()
produce (x:xs) = do
  output x
  produce xs

consume :: Coroutine u t a -> [t]
consume c = 
  case c of
    Done x   -> []
    Out d cr -> d : consume cr
    In h     -> consume (input >>= h)

filterC :: (v -> Bool) -> Coroutine v v ()
filterC p = do
  i <- input
  if p i
    then
    do
      output i
      filterC p
    else filterC p

limit :: Int -> Coroutine v v ()
limit 0 = return ()
limit n = do
  i <- input
  output i
  limit (n - 1)

suppress :: Int -> Coroutine v v ()
suppress 0 = return ()
suppress n = do
  i <- input
  suppress (n - 1)

add :: Coroutine Int Int ()
add = do
  i <- input
  j <- input
  output (i+j)
  add

duplicate :: Coroutine v v ()
duplicate = do
  i <- input
  output i
  output i
  duplicate

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine Int Int ()

p1 = filterC even >>> limit 5
  
p2 = produce [1..] >>> tri
  where
    tri = do
      i <- input
      output (sum [1..i]) -- output i(i+1)/2
      tri
  
p3 = do
  i <- input
  output (2 * i)
  p3


p4 = undefined

