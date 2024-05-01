-- | Codewars: Coroutines
-- | link: https://www.codewars.com/kata/547a77a6b84a1fb8bf000211/train/haskell

{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Coroutine where

import Control.Monad (ap, forever)

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r }
  deriving (Functor)

data Command r u d a = Done a
                     | Out d (Coroutine r u d a)
                     | In (u -> Coroutine r u d a) deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine $ \k -> k (Done x)
  f >>= g  = Coroutine $ \k ->
                           runCoroutine f $ \com -> case com of
                                                      Done a ->
                                                        runCoroutine (g a) k
                                                      In h ->
                                                        runCoroutine (input >>= h >>= g) k
                                                      Out d h ->
                                                        runCoroutine (output d >> h >>= g) k

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $ \k ->
  runCoroutine p2 $ \com ->
                      case com of
                        Done a -> runCoroutine (return a) k
                        In h -> runCoroutine p1 $ \com2 ->
                          case com2 of
                            Done a -> runCoroutine (return a) k
                            Out m g -> runCoroutine (g >>> h m) k
    
  
-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine $ \k ->
                         k $ Out v (Coroutine $ \k' ->
                                                  k' $ Done ())

input :: Coroutine r v d v
input = Coroutine $ \k -> k $ In (\u -> (Coroutine $ \k' -> k $ Done u))

produce :: [a] -> Coroutine r u a ()
produce [] = Coroutine $ \k -> k $ Done ()
produce (x:xs) = Coroutine $ \k -> k $ Out x (produce xs)

consume :: Coroutine [t] u t a -> [t]
consume c = undefined

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = undefined

limit :: Int -> Coroutine r v v ()
limit n = undefined

suppress :: Int -> Coroutine r v v ()
suppress n = undefined

add :: Coroutine r Int Int ()
add = undefined

duplicate :: Coroutine r v v ()
duplicate = undefined

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = undefined
p2 = undefined
p3 = undefined
p4 = undefined
