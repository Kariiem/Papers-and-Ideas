{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

-- | Faster Coroutine Pipelines

module Main where

import Prelude hiding (filter, take, map)
import Prelude qualified as P
import Data.Char
import Data.Void
import System.IO(stdout, hFlush)
import Control.Monad hiding (forever)

main = return ()

infixl 1 |>

class (forall i o. Monad (pipe i o)) => PipeKit pipe where
  input  :: pipe i o i
  output :: o -> pipe i o ()
  (|>)   :: pipe i m () -> pipe m o () -> pipe i o a
  effect :: IO a -> pipe i o a
  exit   :: pipe i o a

data DirectPipe i o a = Input (i -> DirectPipe i o a)
                      | Output o (DirectPipe i o a)
                      | Done a
                      | forall b. Effect (IO b) (b -> DirectPipe i o a)
                      | Exit

instance Functor (DirectPipe i o) where
  fmap f (Done a) = Done (f a)
  fmap f (Input h) = Input (fmap f . h) -- (\u -> fmap f (h u))
  fmap f (Output o p) = Output o (fmap f p)
  
instance Applicative (DirectPipe i o) where
  pure  = return
  (<*>) = ap

instance Monad (DirectPipe i o) where
  return a = Done a
  p >>= f  = case p of
               Exit        -> Exit
               Done a      -> f a
               Input h     -> Input (\u -> h u >>= f)
               Output o q  -> Output o (q >>= f)
               Effect io k -> Effect io (\a -> k a >>= f)

instance PipeKit DirectPipe where
  input  :: DirectPipe i o i
  input =  Input (\u -> return u)

  output :: o -> DirectPipe i o ()
  output o = Output o skip

  effect :: IO a -> DirectPipe i o a
  effect io = Effect io (\a -> return a)

  exit   :: DirectPipe i o a
  exit = Exit

  (|>)   :: DirectPipe i m () -> DirectPipe m o () -> DirectPipe i o a
  x |> y = case y of -- pull pipe
             Done _ -> error "A pipe terminated"
             Exit -> Exit
             Output o q -> Output o (x |> q)
             Input h    -> pipe2 x h
             Effect io k -> Effect io (\u -> x |> k u)

pipe2 :: DirectPipe i t () -> (t -> DirectPipe t o ()) -> DirectPipe i o a
pipe2 p h = case p of
                    Done _     -> error "A pipe terminated"
                    Exit       -> Exit
                    Output o q -> q |> h o
                    Input g    -> Input (\v -> pipe2 (g v) h)
                    Effect io k -> Effect io (\v -> pipe2 (k v) h)

runDirPipe :: (Read i, Show o) => DirectPipe i o () -> IO ()
runDirPipe p = -- the top-level trampoline
  case p of
    Done _      -> skip
    Exit        -> skip
    Input h     -> do v <- readLn; runDirPipe (h v)
    Output v r  -> do print v; runDirPipe r
    Effect io k -> do v <- io; runDirPipe (k v)


upfrom :: PipeKit pipe => Int -> pipe () Int ()
upfrom n = do output n; upfrom (n + 1)

filter :: PipeKit pipe => (a -> Bool) -> pipe a a ()
filter test = forever $ do x <- input; if test x then output x else skip

forever :: Monad m => m a -> m b
forever p = q where q = p >> q

skip :: Monad m => m ()
skip = return ()

primes :: PipeKit pipe => Int -> pipe () Int ()
primes n = upfrom 2 |> sieve |> take n

sieve :: PipeKit pipe => pipe Int Int ()
sieve = do p <- input; output p; filter (\ x -> x `mod` p /= 0) |> sieve

take :: PipeKit pipe => Int -> pipe a a ()
take n = if n == 0 then exit else (do x <- input; output x; take (n - 1))

readLine :: PipeKit pipe => pipe u String ()
readLine = do s <- effect (do putStr "< "; getLine); output s

putLine :: PipeKit pipe => pipe String v ()
putLine = do s <- input; effect (putStrLn ("> " ++ s))

rev :: PipeKit pipe => pipe () () ()
rev = forever readLine |> map reverse |> forever putLine

map :: PipeKit pipe => (a -> b) -> pipe a b ()
map f = forever (do x <- input; output (f x))

