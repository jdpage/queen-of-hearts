{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Numeric.Ring
    ( Ring, Adjoinable, Adjoined()
    , zero, unity
    , negate'
    , (+:), (-:), (*:)
    , apow, unsafeApow, coeff, info, degree
    ) where

class (Eq r, Eq x) => Ring x r | r -> x where
    info :: r -> x
    zero :: x -> r
    negate' :: r -> r
    unity :: x -> r
    (+:) :: r -> r -> r
    (*:) :: r -> r -> r
    
(-:) :: (Ring x r) => r -> r -> r
a -: b   = a +: negate' b

data Adjoined x r a = Adjoined x [r] deriving (Eq)

class Adjoinable a where
    apow :: (Ring x r) => r -> Integer -> Adjoined x r a

unsafeApow :: (Adjoinable a, Ring x r) => r -> Integer -> Adjoined x r a
unsafeApow r p = Adjoined (info r) $ [zero $ info r | _ <- [0..p-1]] ++ [r]

instance (Adjoinable a, Ring x r) => Ring x (Adjoined x r a) where
    info (Adjoined x _) = x
    zero x = Adjoined x []
    unity x = Adjoined x [unity x]

    negate' (Adjoined x rs)   = Adjoined x (negate'' rs) where
        negate'' [] = []
        negate'' (r:rs) = (negate' r):(negate'' rs)

    (Adjoined x1 qs) +: (Adjoined x2 rs)
        | x1 == x2   = Adjoined x1 (add' qs rs)
        | otherwise  = error "mismatch"
        where
            add' [] rs = rs
            add' qs [] = qs
            add' (q:qs) (r:rs) = (q +: r):(add' qs rs)

    (Adjoined x1 qs) *: (Adjoined x2 rs)
        | x1 == x2   = multiply' qs rs
        | otherwise  = error "mismatch"
        where
            multiply' [] rs = Adjoined x1 []
            multiply' qs [] = Adjoined x2 []
            multiply' qs rs = foldl1 (+:) ts where
                qms = zip qs [0..]
                rns = zip rs [0..]
                ts = [(q *: r) `apow` (m + n) | (q, m) <- qms, (r, n) <- rns]

degree :: Adjoined x r a -> Int
degree (Adjoined x rs) = length rs

coeff :: (Ring x r) => Adjoined x r a -> Int -> r
coeff (Adjoined x rs) n
    | n < length rs   = rs !! n
    | otherwise       = zero x

