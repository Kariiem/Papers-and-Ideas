{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs  #-}
-- | Codewars: One function many arguments
-- | link: https://www.codewars.com/kata/599aed42b9712e1afe000014/train/haskell

module PolyvariadicFunctions where



-- `polyAdd` sums its arguments, all `Int`s.

-- polyAdd :: Int -> Int -> ... -> Int
-- polyAdd :: Int -> (Int -> ... -> Int)
-- polyAdd :: Int -> (Int -> (Int -> (Int -> (....))))
-- polyAdd :: Int -> a
-- polyAdd :: Int -> (Int -> a)
--             |
--            Accumulator to serve as return value

class PolyAdd a where
  polyA :: Int -> a
  
instance PolyAdd Int where
  polyA :: Int -> Int
  polyA i = i 

instance (a ~ Int, PolyAdd b) => PolyAdd (a -> b) where
  polyA :: Int -> (a -> b)
  polyA i j = polyA (i+j) 

polyAdd :: (PolyAdd t) => t
polyAdd = polyA 0

-- `polyWords` turns its arguments into a spaced string.
class PolyWords a where
  polyW :: String -> a

instance PolyWords String where
  polyW str = str

instance (a ~ String, PolyWords b) => PolyWords (a -> b) where
  polyW "" str2 = polyW str2
  polyW str1 str2 = polyW (str1 ++ " " ++ str2)

polyWords :: PolyWords t => t
polyWords = polyW ""

-- `polyList` turns its arguments into a list, polymorphically.

-- polyL :: [a] -> a -> a -> ... -> [a]
-- polyL :: [a] -> b
-- polyL :: [a] -> (a -> b)

class PolyList a b | b -> a where
  polyL :: [a] -> b

instance PolyList a [a] where
  polyL :: [a] -> [a]
  polyL xs = xs

instance (PolyList a b) => PolyList a (a -> b) where
  polyL :: [a] -> (a -> b)
  polyL as a = polyL (as ++ [a])

polyList :: PolyList a b => b
polyList = polyL []

