{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Functional Programming with Overloading
-- | and Higher-Order Polymorphism

module Main where

import Prelude hiding ( Functor(..)
                      , Monad
                      , (>>)
                      , succ
                      , pred
                      , foldr
                      , foldl)

main = return ()

-----------------------------------------------------------
-- Duality and the De Morgan Principle

class Dual a where
  -- law: dual . dual  = id
  -- law: dual (f x)   = (dual f) (dual x)
  -- law: dual (f . g) = (dual f) . (dual g)
  dual :: a -> a

instance Dual Bool where
  dual = not

instance (Dual a, Dual b) => Dual (a->b) where
  -- (dual . dual) f
  -- = dual (dual f)
  -- = dual (dual . f . dual)
  -- = dual . dual . f . dual . dual
  -- = id . f . id
  -- = f

  -- (dual f) (dual a)
  -- = dual . f . dual . dual $ a
  -- = dual . f $ a
  -- = dual (f a)

  -- dual (f . g)
  -- = dual . f . g . dual
  -- = dual . f . id .g .dual
  -- = dual . f . dual . dual . g .dual
  -- = dual f . dual g
  dual f = dual . f .dual

-- example : dual (&&) = (||)
-- dual (&& :: Bool -> Bool -> Bool) : Dual (a' -> (a -> b))
-- = dual . (&&) . dual
-- = dual . (&&) . not
-- = \x -> dual ((&&) (not x))
-- = \x -> dual (x_ &&)
-- = \x -> dual . (x_ &&) . dual
-- = \x -> not . (x_ &&) . not
-- = \x -> (\y -> not ((x_ &&) (not y)))
-- = \x -> (\y -> not (x_ && y_))
-- = \x y -> not (x_ && y_)
-- = \x y -> x || y {De Morgan's principle}

-- (&&) True x = x
-- (&&) False x = False

-- (dual (&&)) False x = x
-- (dual (&&)) True x = True

instance {-# Overlaps #-} (Num t, Enum t) => Dual t where
  dual = negate

instance Dual a => Dual [a] where
  dual = reverse . map dual

-- dual head  = last
-- dual tail  = init
-- dual (++)  = flip (++)
-- dual (+)   = (+)
-- dual max   = min
-- dual min   = max
-- dual foldl = foldr . flip
-- dual foldr = foldl . flip

-----------------------------------------------------------
-- Computing with Lattices
class (Eq a) => Lattice a where
  bottom, top :: a
  meet, join :: a -> a -> a
  lt :: a -> a -> Bool
  x `lt` y = (x `join` y) == y

instance Lattice Bool where
  bottom = False
  top = True
  meet = (&&)
  join = (||)

instance (Lattice a, Lattice b) => Lattice (a, b) where
  bottom = (bottom, bottom)
  top = (top, top)
  (a, b) `meet` (a', b') = (a `meet` a', b `meet` b')
  (a, b) `join` (a', b') = (a `join` a', b `join` b')

fix :: Lattice a => (a -> a) -> a
fix f = firstRepeat (iterate f bottom)
  where
    firstRepeat :: Eq a => [a] -> a
    firstRepeat (x:xs) = if x == head xs
                          then x
                          else firstRepeat xs

------------------------------------------------------------
-- Recursion schemes
newtype Mu f = In (f (Mu f))
deriving instance (Show (f (Mu f))) => Show (Mu f)

out :: Mu f -> f (Mu f)
out (In x) = x

data NatF s = Zero | Succ s
type Nat = Mu NatF

instance Functor NatF where
  fun f Zero = Zero
  fun f (Succ x) = Succ (f x)

zero :: Nat
zero = In Zero

succ :: Nat -> Nat
succ x = In (Succ x)

data ListF a s = Nil | Cons a s
  deriving (Show)
type List a = Mu (ListF a)

instance Functor (ListF t) where
  fun :: (a -> b) -> ListF t a -> ListF t b
  fun f Nil = Nil
  fun f (Cons a as) = Cons a (f as)

cata :: Functor f => (f a -> a) -> Mu f -> a
cata phi = phi . fun (cata phi) . out

ana :: Functor f => (a -> f a) -> a -> Mu f
ana psi = In . fun (ana psi) . psi

len = cata (\fa -> case fa of
               Nil       -> zero
               Cons z zs -> succ zs)

intsFrom = ana (\n -> Cons n (n+1))

------------------------------------------------------------
class Functor f where
  fun :: (a -> b) -> f a -> f b

class (Functor m) => Monad m where
  result :: a -> m a
  bind :: m a -> (a -> m b) -> m b

newtype Id a = Id a

instance Functor Id where
  fun f (Id x) = Id (f x)

newtype State s a = State {runState :: s -> (a, s)}
instance Functor (State s) where
  fun :: (a -> b) -> State s a -> State s b
  fun f st = State $ \s -> let (a, s') = runState st s
                               in (f a, s')

instance Monad (State s) where
  result x = State $ \s -> (x, s)
  m `bind` f = State $ \s -> let (a, s') = runState m s
                             in runState (f a) s'

newtype Writer s a = Writer (s, a)

instance Functor (Writer s) where
  fun f (Writer (s, x)) = Writer (s, f x)

instance (Monoid s) => Monad (Writer s) where
  result x = Writer (mempty, x)
  m `bind` f = let Writer (s, x) = m
                   Writer (s', y) = f x
               in Writer (s <> s', y)

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fun f g = Reader $ f . runReader g

instance Monad (Reader r) where
  result x = Reader $ \_ -> x
  m `bind` f = Reader $ \r -> let a = runReader m r
                              in runReader (f a) r

instance Functor ((->) r) where
  fun f g = f . g

instance Monad ((->) r) where
  result x = \r -> x
  bind m f = \r -> f (m r) r -- S combinator flipped

class Monad m => MonadState s m where
  update :: (s -> s) -> m s

instance MonadState s (State s) where
  update f = State $ \s -> (s, f s)

incr :: MonadState Int m => m Int
incr = update (1+)

random :: MonadState Int m => Int -> m Int
random n = update min_stand `bind` \m -> result (m `mod` n)

min_stand n = if test > 0
              then test
              else test + 2147483647
  where
    test = 16807 * lo - 2863 * hi
    hi = n `div` 127773
    lo = n `mod` 127773

class (Monad m, Monoid w) => MonadWriter w m where
  write :: w -> m ()

instance Monoid w => MonadWriter w (Writer w) where
  write w = Writer (w, ())

class (Monad m) => MonadReader r m where
  ask   :: m r
  env :: r -> m a -> m a

instance MonadReader r ((->) r) where
  ask = id
  env e c = \_ -> c e

nxt m = update (m+) `bind` \n -> if n > 0
                                 then write ("count = " ++ show n)
                                 else fail "count must be positive"

data Type v = TVar v
            | TInt
            | Fun (Type v) (Type v)

instance Show v => Show (Type v) where
  showsPrec _ (TVar v)  = shows v
  showsPrec _ TInt      = showString "Int"
  showsPrec p (Fun l r) = showParen (p > 0) str
    where
      str = showsPrec 1 l . showString " -> " . shows r

instance Functor Type where
  fun f TInt      = TInt
  fun f (TVar x)  = TVar (f x)
  fun f (Fun l r) = Fun (fun f l) (fun f r)

instance Monad Type where
  result v = TVar v
  TVar v    `bind` f = f v
  TInt      `bind` f = TInt
  (Fun l r) `bind` f = Fun (l `bind` f) (r `bind` f)

apply :: Monad m => (a -> m b) -> m a -> m b
apply s t = t `bind` s

(>=>) :: Monad m => (a -> m b) -> (c -> m a) -> (c -> m b)
f >=> g = (`bind` f) . g

type Subst m v = v -> m v

(>>) :: (Eq v, Monad m) => v -> m v -> Subst m v
(v >> t) w = if v == w then t else result w


unify :: (Monad m, Eq a, MonadFail m, Show a) =>
         Type a -> Type a -> m (Subst Type a)
unify TInt TInt =
  result result
unify (TVar v) (TVar w) =
  result (if v==w then result else v >> (TVar v))
unify (TVar v) t =
  varBind v t
unify t (TVar v) =
  varBind v t
unify (Fun d r) (Fun e s) =
  unify d e `bind` \s1 ->
  unify (apply s1 r) (apply s1 s) `bind` \s2 ->
  result (s2 >=> s1)
unify t1 t2 =
  fail ("Cannot unify " ++ show t1
         ++ " with " ++ show t2)

varBind :: (Eq v, MonadFail m, Monad m) =>
           v -> Type v -> m (Subst Type v)
varBind v t =
  if (v `elem` vars t)
  then fail "Occurs check fails"
  else result (v >> t)

  where vars (TVar v)  = [v]
        vars TInt      = []
        vars (Fun d r) = vars d ++ vars r
------------------------------------------------------------
newtype FComp m n a = FC { unFC :: (n (m a)) }
newtype BComp m n a = BC { unBC :: (m (n a)) }

instance (Functor m, Functor n) => Functor (FComp m n) where
  fun f (FC c) = FC (fun (fun f) c)

instance (Functor m, Functor n) => Functor (BComp m n) where
  fun f (BC c) = BC (fun (fun f) c)

-- Condition for monad composition
-- swap :: m (n a) -> n (m a)
class Monad m => Into m where
  into :: Monad n => m (n a) -> n (m a)

instance (Into m, Monad n) => Monad (FComp m n) where
  result x = FC (result (result x))
  (FC c) `bind` f = FC ((fun join . join . fun f') c)
    where
      f' = into . fun (unFC . f)
      join mm = mm `bind` id

instance Functor Maybe where
  fun f Nothing = Nothing
  fun f (Just x) = Just (f x)

instance Monad Maybe where
  result = Just
  Nothing `bind` f = Nothing
  (Just x) `bind` f = f x

instance Into Maybe where
  into Nothing = result Nothing
  into (Just c) = fun Just c

instance (Monoid s) => Into (Writer s) where
  into (Writer (s, a)) = a `bind` \x -> result (Writer (s, x))

class Monad m => OutOf m where
  outof :: Monad n => n (m a) -> m (n a)
