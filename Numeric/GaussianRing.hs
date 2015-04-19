{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Numeric.GaussianRing
    ( Zn(), Zni()
    , modulo
    , makeZn, makeZni
    , onex, ix
    , modulus
    , re, im, re', im'
    , conjugate
    , magnitude
    , argument
    , enumerateZni
    , showPolar
    , module Numeric.Ring
    ) where

import Text.Printf
import Numeric.Ring

data Zn a = Zn a a deriving (Show, Eq)

modulo :: (Integral a) => a -> a -> a
modulo a b = ((a `mod` b) + b) `mod` b

makeZn :: (Integral a) => a -> a -> Zn a
makeZn n r = Zn n $ r `modulo` n

instance (Integral a) => Ring a (Zn a) where
    info (Zn n _) = n
    zero n = makeZn n 0
    unity n = makeZn n 1
    negate' (Zn n r) = makeZn n (-r)
    (Zn n1 r1) +: (Zn n2 r2)
        | n1 == n2   = makeZn n1 $ r1 + r2
        | otherwise  = error "moduli must be equal"
    (Zn n1 r1) *: (Zn n2 r2)
        | n1 == n2   = makeZn n1 $ r1 * r2
        | otherwise  = error "moduli must be equal"

data I = I
type Zni a = Adjoined a (Zn a) I

instance Adjoinable I where
    r `apow` p
        | p `modulo` 4 == 0   = r `unsafeApow` 0
        | p `modulo` 4 == 1   = r `unsafeApow` 1
        | p `modulo` 4 == 2   = (negate' r) `unsafeApow` 0
        | otherwise           = (negate' r) `unsafeApow` 1

onex :: (Integral a) => Zn a -> Zni a
onex r = r `apow` 0

ix :: (Integral a) => Zn a -> Zni a
ix i = i `apow` 1

makeZni :: (Integral a) => a -> a -> a -> Zni a
makeZni n r i = (onex $ makeZn n r) +: (ix $ makeZn n i)

modulus :: (Integral a) => Zni a -> a
modulus z = info z

re :: (Integral a) => Zni a -> Zn a
re z = z `coeff` 0

im :: (Integral a) => Zni a -> Zn a
im z = z `coeff` 1

re' :: (Integral a) => Zni a -> Zni a
re' = onex . re

im' :: (Integral a) => Zni a -> Zni a
im' = ix . im

conjugate :: (Integral a) => Zni a -> Zni a
conjugate z = re' z -: im' z

magnitude :: (Floating a) => Zni Integer -> a
magnitude z =
    let (Zn _ r) = re z in
    let (Zn _ i) = im z in
    (fromInteger $ r * r + i * i) ** 0.5

argument :: (Floating a) => Zni Integer -> a
argument z =
    let (Zn _ r) = re z in
    let (Zn _ i) = im z in
    atan $ (fromInteger $ r) / (fromInteger $ i)

enumerateZni :: (Integral a) => a -> [Zni a]
enumerateZni n = let ns = [0..(n - 1)] in [makeZni n r i | r <- ns, i <- ns]

showPolar :: Zni Integer -> String
showPolar z = printf "%.2f,%.2f;" (magnitude z :: Float) (argument z :: Float)

