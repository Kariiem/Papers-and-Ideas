-- | Defunctionalization of CPS

module Main where

main = return ()

run :: Int -> Int -> Bool -> Int
run x y b = aux (\z -> x + z) *
              aux (\z -> if b
                         then y + z
                         else y - z)
  where
    aux :: (Int -> Int) -> Int
    aux f = f 1 + f 10

data Lam = Lam1 Int
         | Lam2 Int Bool

run2 :: Int -> Int -> Bool -> Int
run2 x y b = aux_def (Lam1 x) * aux_def (Lam2 y b)
  where
    aux_def l = apply l 1 + apply l 10
    
    apply :: Lam -> Int -> Int
    apply (Lam1 x) z = x + z
    apply (Lam2 y b) z = if b
                         then y + z
                         else y - z



------------------------------------------------------------
-- dynamic
run3 i xs = walk xs
  where
    walk :: [Int] -> [Int]
    walk [] = []
    walk (y:ys) = aux i (\e -> e + y): walk ys

    aux :: Int -> (Int -> Int) -> Int
    aux i f = f i

data Lam2 = L Int

run4 i xs = walk xs
  where
    walk :: [Int] -> [Int]
    walk [] = []
    walk (y:ys) = aux i (L y): walk ys

    aux :: Int -> Lam2 -> Int
    aux i l = apply l i

    apply (L y) e = e + y 

rec0 :: [Int] -> Bool
rec0 xs = case walk xs of
            Just ls -> null ls
            Nothing -> False

  where
    walk (0:ys) = case walk ys of
                    Just (1:zs) -> Just zs
                    _           -> Nothing
    walk ys = Just ys
      

rec1 xs = walk xs (\ls -> null ls)
  where
    walk (0 : ys) k = walk ys (\zs -> case zs of
                                        (1:zs') -> k zs'
                                        _       -> False)
    walk ys k = k ys
                      
data Cont = Cont0
          | Cont1 Cont

rec2 xs = walk xs Cont0
  where
    walk :: [Int] -> Cont -> Bool
    walk (0:ys) k = walk ys (Cont1 k)
    walk ys k = apply k ys

    apply :: Cont -> [Int] -> Bool
    apply Cont0 ls = null ls
    apply (Cont1 c) (1:ls) = apply c ls
    apply (Cont1 c) _ = False

rec3 :: [Int] -> Bool
rec3 xs = walk xs 0
  where
    walk :: [Int] -> Int -> Bool
    walk (0:ys) n = walk ys (n+1)
    walk ys n = apply n ys

    apply :: Int -> [Int] -> Bool
    apply 0 ls = null ls
    apply n (1:ls) = apply (n - 1) ls
    apply n  _ = False

