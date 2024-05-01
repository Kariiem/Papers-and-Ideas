-- | Programming in the λ-Calculus: From Church to Scott and Back

module Main where

main = return ()


data Temperature = Fahrenheit Int | Celsius Int deriving (Eq, Show)

fahrenheit = \t f c -> f t
celsius = \t f c -> c t

warm :: Temperature -> Bool
warm (Fahrenheit f) = f > 90
warm (Celsius c) = c > 30

newtype TemperatureC = TempC
  { unTempC :: forall r.
               (Int->r)
            -> (Int->r)
            -> r
  }

fromCtoN :: TemperatureC -> Temperature
fromCtoN tc = unTempC tc toFahrenheit toCelsius
  where
    toFahrenheit = Fahrenheit
    toCelsius = Celsius

mkF :: Int -> TemperatureC
mkF t = TempC $ \f c -> f t

mkC :: Int -> TemperatureC
mkC t = TempC $ \f c -> c t

warmC :: TemperatureC -> Bool
warmC tc = unTempC tc (\f -> f > 90) (\c -> c > 30)

data Optional a = None
                | Some a

none = \n s -> n
some = \a n s -> s a

newtype OptionalC a = OptionalC
  { unOpt :: forall r.
             r
          -> (a->r)
          -> r
  }
true = \t f -> t
false = \t f -> f

newtype BoolC = BoolC { unBool :: forall r. r -> r -> r }

trueC = BoolC $ \t f -> t
falseC = BoolC $ \t f -> f

toBoolN bc = unBool bc True False


newtype Tuple a b = Tuple (forall r. (a -> b -> r) -> r)

mkTuple a b = Tuple $ \f -> f a b
fstT (Tuple t) = t (\a b -> a)
secondT (Tuple t) = t (\a b -> b)


-- Scott Encoding


data Nat = Zero | Succ Nat deriving (Eq, Show)

newtype NatS = NatS {unNatS::forall r. r -> (NatS -> r) -> r}

instance Show NatS where
  show (NatS n) = n "zero" (\m-> "succ (" ++ show m ++ ")")

zeroS :: NatS
zeroS = NatS $ \z s -> z

succS :: NatS -> NatS
succS n = NatS $ \z s -> s n

predS :: NatS -> NatS
predS (NatS n) = n undefined (\m -> m)

data List a = Nil | Cons a (List a) deriving (Eq, Show)



newtype ListS a = ListS {unListS::forall r. r -> (a -> ListS a -> r) -> r}

instance Show a => Show (ListS a) where
  show (ListS xs) = xs "nil" (\x xs -> show x ++ ":" ++ show xs)

nilS :: ListS a
nilS = ListS $ \n c -> n

consS :: a -> ListS a -> ListS a
consS x xs = ListS $ \n c -> c x xs

headS :: ListS a -> a
headS (ListS xs) = xs undefined (\y ys -> y)

tailS :: ListS a -> ListS a
tailS (ListS xs) = xs undefined (\y ys -> ys)
