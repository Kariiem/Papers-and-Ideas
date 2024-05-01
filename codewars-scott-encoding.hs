-- | Codewars: Scott-Encoding
-- | link: https://www.codewars.com/kata/59c132fb70a3b7efd3000024/train/haskell

{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair p = runPair p (,)
fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair $ \f -> f a b
fst :: SPair a b -> a
fst p = runPair p (\a _ -> a)
snd :: SPair a b -> b
snd p = runPair p (\_ b -> b)
swap :: SPair a b -> SPair b a
swap p = SPair $ runPair p . flip
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \a b -> let p = fromPair (a,b) in f p
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \p -> let (a,b) = toPair p in f a b

toMaybe :: SMaybe a -> Maybe a
toMaybe s = runMaybe s Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe m = SMaybe $ \n j -> case m of Nothing -> n ; Just x -> j x
isJust :: SMaybe a -> Bool
isJust m = runMaybe m False (\_ -> True)
isNothing :: SMaybe a -> Bool
isNothing m = runMaybe m True (\_ -> False)
catMaybes :: SList (SMaybe a) -> SList a
catMaybes ms = runList ms mnil (\z zs -> let czs = catMaybes zs
                                        in runMaybe z czs (\a -> cons a czs))

toEither :: SEither a b -> Either a b
toEither e = runEither e Left Right
fromEither :: Either a b -> SEither a b
fromEither e = SEither $ \l r -> case e of Left x -> l x; Right x -> r x
isLeft :: SEither a b -> Bool
isLeft e = runEither e (\_ -> True) (\_ -> False)
isRight :: SEither a b -> Bool
isRight e = runEither e (\_ -> False) (\_ -> True)
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition ls = foldr step (fromPair (mnil, mnil)) ls
  where
    step :: SEither a b -> SPair (SList a) (SList b) -> SPair (SList a) (SList b)
    step e ls_rs = runEither e
                 (\l -> runPair ls_rs (\f s -> fromPair (l `cons` f, s)))
                 (\r -> runPair ls_rs (\f s -> fromPair (f, r `cons` s)))

toList :: SList a -> [a]
toList xs = runList xs [] (\a ls -> a : toList ls)
fromList :: [a] -> SList a
fromList xs = SList $ \n ls -> case xs of [] -> n
                                          y:ys -> ls y (fromList ys)
cons :: a -> SList a -> SList a
cons x xs = SList $ \_ ls -> ls x xs
concat :: SList a -> SList a -> SList a
concat xs ys = runList xs ys (\z zs ->
                                 runList ys xs (\_ _ ->
                                                   cons z (concat zs ys)))
null :: SList a -> Bool
null xs = runList xs True (\_ _ -> False)
length :: SList a -> Int
length xs = runList xs 0 (\_ ls -> 1 + length ls)
map :: (a -> b) -> SList a -> SList b
map f xs = runList xs mnil (\z zs -> cons (f z) (map f zs))
zip :: SList a -> SList b -> SList (SPair a b)
zip xs ys =
  runList xs mnil (\a as ->
                    runList ys mnil (\b bs ->
                                       cons (fromPair (a, b)) (zip as bs)))
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f e xs = runList xs e (\z zs -> foldl f (f e z) zs)
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f e xs = runList xs e (\z zs -> f z (foldr f e zs)) 
take :: Int -> SList a -> SList a
take n xs = runList xs mnil (\z zs -> if n == 0
                                     then mnil
                                     else cons z (take (n-1) zs))

mnil = fromList []
