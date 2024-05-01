{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Data types à la carte

module Datatypes where

data Expr f = In (f (Expr f))
deriving instance (forall a. Show a => Show (f a)) => Show (Expr f)

data Lit a = Lit Int
  deriving (Eq, Show)

data Add a = Add a a
  deriving (Eq, Show)

infixr :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

instance Functor Lit where
  fmap f (Lit i) = (Lit i)

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Lit where
  evalAlgebra (Lit i) = i

instance Eval Add where
  evalAlgebra (Add e1 e2) = e1 + e2

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl e) = evalAlgebra e
  evalAlgebra (Inr e) = evalAlgebra e

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr


class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl x) = Just x
  prj (Inr x) = Nothing

instance {-# Overlaps #-} (Functor f,
          Functor g,
          Functor h,
          f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj (Inl x) = Nothing
  prj (Inr x) = prj x

{-
Instance resolution in Haskell:

- Only looks at the instance head for a match, not the context.
  The context is checked after the match.

- Never backtracks when it finds an unsatisfied constraint after a match.
  It just fails.

- Doesn't assume it knows about all the instances of a given typeclass (open world
  assumption). This means it cannot reason like this: "well, there's only one instance
  declared for MyTypeclass, so I guess that must be the type".
-}

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

lit :: (Lit :<: f) => Int -> Expr f
lit i = inject (Lit i)

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add e1 e2 = inject (Add e1 e2)


x::Expr (Add :+: Lit) = lit 1 `add` lit 2

inLit :: Int -> Expr (Lit :+: Lit)
inLit i = inject (Lit i)
