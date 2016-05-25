{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List ( intercalate )

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P as) (P []) = all (==0) as
    (==) (P []) (P bs) = all (==0) bs
    (==) (P (a:as)) (P (b:bs)) = (a == b) && ((P as) == (P bs))
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P as) = handleZero $ intercalate " + " $ reverse $ filter (not . null) $ zipWith combine as [(0::Int)..]
      where
        combine 0 _ = ""
        combine c 0 = show c
        combine (-1) 1 = "-x"
        combine 1 1 = "x"
        combine (-1) e = "-x^" ++ (show e)
        combine 1 e = "x^" ++ (show e)
        combine c 1 = (show c) ++ "x"
        combine c e = (show c) ++ "x^" ++ (show e)

        handleZero "" = "0"
        handleZero str = str

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P cs) (P ds) = P (addLists cs ds)
    where
        addLists [] [] = []
        addLists as [] = as
        addLists [] bs = bs
        addLists (a:as) (b:bs) = (a+b):(addLists as bs)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P as) (P bs) = sumPoly $ undefined
    where
        sumPoly = foldr (+) (P [0])

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
