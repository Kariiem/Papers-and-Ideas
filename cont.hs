-- The zipper, functional pearl
-- Clowns to the left me, jokers to the right
-- Hutton's Razor
-- Calculating Correct Compilers
-- Deriving Target Code as
--    a Representation of Continuation Semantics
-- Definitional Interpereters for Higher Order Programming
--    Languages
-- A Functional Correspondance between evaluators and
--    abstract machines

module Main where

import Data.Typeable
import Debug.Trace
import Data.List as L

main = return ()

newtype Cont a = Cont { unCont::(forall r. (a -> r) -> r) }

instance Functor Cont where
  fmap f (Cont arr) = Cont $ \br -> arr (br . f)

instance Applicative Cont where
  pure x = Cont $ \h -> h x
  (Cont f) <*> (Cont a) = Cont $ \h -> h (f a)

-- f   :: ((a->b)->r) -> r
-- a   ::  (a->r)->r
-- f a :: b

instance Monad Cont where
  (Cont m) >>= f = Cont $ \h -> m $ \a -> unCont (f a) h

-- m :: (a -> r) -> r
-- f :: a -> (b -> r) -> r


fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

-- fact2' n k = k (fact2 n)

(-&) :: Integer -> Integer -> (Integer -> Integer) -> Integer
(-&) x y k = k (x - y) 

(*&) :: Integer -> Integer -> (Integer -> Integer) -> Integer
(*&) x y k = k (x * y) 


fact2 :: Integer -> Integer
fact2 n = fact' n id
  where
    fact':: Integer -> (Integer -> Integer) -> Integer
    fact' 0 k = k 1
    fact' n k = fact' (n - 1) (\m -> trace ("fact " ++ show n)
                                     $ k . (n*) $ m)

{-
(-&) n 1 (\n1 ->
              fact2' n1 (\fn1 ->
                             (*&) n fn1 k)))
-}
    
fact3 :: Integer -> Integer
fact3 n = fact' n []
  where
    fact':: Integer -> [Integer]  -> Integer
    fact' 0 k = product k 
    fact' n k = fact' (n - 1) (k ++ [n])


fact4 :: Integer -> Integer
fact4 n = fact' n 1
  where
    fact':: Integer -> Integer  -> Integer
    fact' 0 k = k 
    fact' n k = fact' (n - 1) (n * k)


reverse1::[a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]


(++&) x y k = k (x ++ y)

(#&) x xs k = k (x:xs)

tailCont xs k = k (tail xs)
headCont xs k = k (head xs)


reverse2 xs = reverse' xs id
  where
    reverse' []     k = k []
    reverse' (x:xs) k = reverse' xs (\zs -> k (zs ++ [x]))


reverse3a xs = reverse' xs id
  where
    reverse' :: [a] -> ([a] -> [a]) -> [a]
    reverse' []     k = k []
    reverse' (x:xs) k = reverse' xs (\rxs -> k . (++ [x]) $ rxs)

reverse3 xs = reverse' xs []
  where
    reverse' :: [a] -> [a] -> [a]
    reverse' []     k = k
    reverse' (x:xs) k = reverse' xs (x:k)


data Tree a = Tip a | Bin (Tree a) (Tree a)
  deriving (Eq, Show)

toTree :: Ord a => [a] -> Tree a
toTree [x] = Tip x
toTree xs = Bin (toTree l) (toTree r)
  where
    sorted = L.sort xs
    (l, r) = splitAt (length xs `div` 2)  xs

flatten :: Tree a -> [a]
flatten (Tip a) = [a]
flatten (Bin l r) = flatten l ++ flatten r

flatten1 :: Tree a -> [a]
flatten1 ts = flatten' ts id
  where
    flatten' :: Tree a -> ([a] -> [a]) -> [a]
    flatten' (Tip a)   k = k [a]
    flatten' (Bin l r) k = flatten' l
                           (\ls ->
                               flatten' r
                               (\rs ->
                                   k (ls ++ rs)))
              
-- (++&) x y k = k (x ++ y)
flatten2a :: Tree a -> [a]
flatten2a ts = flatten' ts id
  where
    flatten' :: Tree a -> ([a] -> [a]) -> [a]
    flatten' (Tip a)   k = k [a]
    flatten' (Bin l r) k = flatten' l
                           (\ls ->
                               flatten' r
                               (\rs ->
                                   k . (ls ++) $ rs))



flatten2b :: Tree a -> [a]
flatten2b ts = flatten' ts id
  where
    flatten' :: Tree a -> ([a] -> [a]) -> [a]
    flatten' (Tip a)   k = k [a]
    flatten' (Bin l r) k = flatten' r
                           (\rs ->
                               flatten' l
                               (\ls ->
                                   k (ls ++ rs)))

-- defunctionalization
data L a = Lam0
         | Lam1 (Tree a) (L a)
         | Lam2 [a] (L a)


flatten2 :: Tree a -> [a]
flatten2 ts = flatten' ts Lam0
  where
    flatten' :: Tree a -> L a -> [a]
    flatten' (Tip a)   k = apply k [a]
    flatten' (Bin l r) k = flatten' r (Lam1 l k)

    apply :: L a -> [a] -> [a]
    apply Lam0 xs = xs
    apply (Lam1 l k) rs = flatten' l (Lam2 rs k)
    apply (Lam2 rs k) ls = apply k (ls ++ rs)


-- data L a = Lam0
--          | Lam1 (Tree a) (L a)
--          | Lam2 [a] (L a)

-- Isomorphic to : [Either (Tree a) ([a])]

type ContRep a = [Either [a] (Tree a)]

flatten3 :: Tree a -> [a]
flatten3 ts = flatten' ts []
  where
    flatten' :: Tree a -> ContRep a -> [a]
    flatten' (Tip a)   k = apply k [a]
    flatten' (Bin l r) k = flatten' r (Right l:k)

    apply :: ContRep a -> ([a] -> [a])
    apply [] = id
    apply (Left ys : k) = \xs -> apply k (xs ++ ys)
    apply (Right l : k) = \xs -> flatten' l (Left xs: k)



type ContRep2 a = ([a] ,[(Tree a)])
flatten4 :: Tree a -> [a]
flatten4 ts = flatten' ts ([], [])
  where
    flatten' :: Tree a -> ContRep2 a -> [a]
    flatten' (Tip a)   (ys, ts) = apply (ys, ts) [a]
    flatten' (Bin l r) (ys, ts) = flatten' r (ys, l:ts)

    apply :: ContRep2 a -> ([a] -> [a])
    apply (ys, []) = \xs -> xs ++ ys
    apply (ys, t:ts) = \xs -> flatten' t (xs ++ ys, ts)

  
flatten5 :: Tree a -> [a]
flatten5 ts = flatten' [ts] []
  where
    flatten' :: [Tree a] -> [a] -> [a]
    flatten' [Tip x]      ys = x:ys
    flatten' (Tip x:ts)   ys = flatten' ts (x:ys)
    flatten' (Bin l r:ts) ys = flatten' (r:l:ts) ys

  
fib n = fib' n id
  where
    fib' 0 k = trace ("fib' " ++ show 0) $ k 0
    fib' 1 k = trace ("fib' " ++ show 1) $ k 1
    fib' n k = trace ("fib' " ++ show n)
               $ fib' (n - 1) (\fn1 ->
                                 fib' (n - 2) (\fn2 ->
                                                 k (fn1 + fn2)))

data Fib = F0 | F1 Int Fib | F2 Int Fib
fib2 n = fib' n F0
  where
    fib' 0 k = apply k 0
    fib' 1 k = apply k 1
    fib' n k = fib' (n - 1) (F1 (n-2) k)

    apply :: Fib -> Int -> Int
    apply F0 n = n
    apply (F1 m k) n = fib' m (F2 n k)
    apply (F2 m k) n = apply k (n + m)

fib3 n = fib' n []
  where
    fib' 0 k = apply k 0
    fib' 1 k = apply k 1
    fib' n k = fib' (n - 1) (Left (n-2):k)

    apply :: [Either Int Int] -> Int -> Int
    apply [] n = n
    apply (Right m:k) n = apply k (n + m)
    apply (Left m:k) n = fib' m (Right n:k)


fib4 n = fib' n (0, [])
  where
    fib' :: Int -> (Int, [Int]) -> Int
    fib' 0 (m, ls) = apply (m, ls) 0
    fib' 1 (m, ls) = apply (m, ls) 1
    fib' n (m, ls) = fib' (n - 1) (m, (n-2):ls)

    apply :: (Int, [Int]) -> Int -> Int
    apply (m, [])   n = n + m
    apply (m, l:ls) n = fib' l (n + m, ls)


fib5 n = fib' [n] 0
  where
    fib' :: [Int] -> Int -> Int
    fib' [0]    m = trace "fib' 0"      $ m
    fib' [1]    m = trace "fib' 1"      $ m + 1
    fib' (0:ls) m = trace "fib' (0:ls)" $ fib' ls m
    fib' (1:ls) m = trace "fib' (1:ls)" $ fib' ls (m + 1)
    fib' (l:ls) m = trace "fib' (l:ls)" $ fib' ((l - 1) : (l-2):ls) m


fib6 n = fib' n 0 1
  where
    fib' 0 a b = a
    fib' 1 a b = b
    fib' n a b = fib' (n - 1) b (a+b)
{-
0, 1, 1, 2, 3, 5, 8, 13.....
-------
fib4 5

fib' 5 [] []


-}
