module Music
    ( Pitch(..)
    , PitchClass()
    , Key()
    , Accidental(..)
    , Octave(..)
    , pcC, pcCis, pcD, pcDis, pcE, pcF, pcFis, pcG, pcGis, pcA, pcAis, pcB
    , pcAes, pcBes, pcDes, pcEes, pcGes
    , keyGes, keyDes, keyAes, keyEes, keyBes, keyF, keyC, keyG, keyD, keyA
    , keys
    , keyE, keyB, keyFis
    , keyEesm, keyBesm, keyFm, keyCm, keyGm, keyDm, keyAm, keyEm, keyBm
    , keyFism, keyCism, keyGism, keyDism 
    , pitchClassOf, octaveOf
    , pitch
    , sharp, flat, sharps, flats
    , fifth, fifth'
    , naturals
    , isNatural, naturalOfFlat, naturalOfSharp
    , sharpCount
    , keySignature, keyPitches, inKey
    ) where

import Data.Cycle (Cycle)
import qualified Data.Cycle as Cycle
import Numeric.GaussianRing (modulo)

data Pitch = Pitch Int deriving (Eq, Show) -- midi number
data PitchClass = PitchClass Int deriving (Eq, Show)
data Key = Key Int deriving (Eq, Show)
data Accidental = Sharp | Natural | Flat deriving (Eq, Show)

type Octave = Int

pcC   = PitchClass 0
pcCis = PitchClass 1
pcD   = PitchClass 2
pcDis = PitchClass 3
pcE   = PitchClass 4
pcF   = PitchClass 5
pcFis = PitchClass 6
pcG   = PitchClass 7
pcGis = PitchClass 8
pcA   = PitchClass 9
pcAis = PitchClass 10
pcB   = PitchClass 11

pcAes = pcGis
pcBes = pcAis
pcDes = pcCis
pcEes = pcDis
pcGes = pcFis

keyGes = Key (-6)
keyDes = Key (-5)
keyAes = Key (-4)
keyEes = Key (-3)
keyBes = Key (-2)
keyF = Key (-1)
keyC = Key 0
keyG = Key 1
keyD = Key 2
keyA = Key 3
keyE = Key 4
keyB = Key 5
keyFis = Key 6

keys = map Key $ [0,(-1)..(-6)] ++ [1..6]

keyEesm = keyGes
keyBesm = keyDes
keyFm = keyAes
keyCm = keyEes
keyGm = keyBes
keyDm = keyF
keyAm = keyC
keyEm = keyG
keyBm = keyD
keyFism = keyA
keyCism = keyE
keyGism = keyB
keyDism = keyFis

octaveOf :: Pitch -> Octave
octaveOf (Pitch n) = n `div` 12 - 1

pitch :: Octave -> PitchClass -> Pitch
pitch octave (PitchClass pc) = Pitch $ (octave + 1) * 12 + pc

class (Eq p) => Pitch' p where
    sharps :: Int -> p -> p

    flats :: Int -> p -> p
    flats k = sharps (-k)

    sharp :: p -> p
    sharp = sharps 1

    flat :: p -> p
    flat = flats 1

    pitchClassOf :: p -> PitchClass

fifth :: (Pitch' p) => p -> p
fifth = sharps 7 

fifth' :: (Pitch' p) => p -> p
fifth' = flats 7

instance Pitch' PitchClass where
    sharps k (PitchClass n) = PitchClass $ (n + k) `modulo` 12
    pitchClassOf n = n

instance Pitch' Pitch where
    sharps k (Pitch n) = Pitch $ n + k
    pitchClassOf (Pitch n) = PitchClass $ n `modulo` 12

instance Pitch' Key where
    sharps k (Key n)
        | (-6) <= m && m <= 6   = Key m
        | otherwise             = error "invalid key"
        where m = n + k
    pitchClassOf (Key n) = PitchClass $ (n * 7) `modulo` 12

naturals :: [PitchClass]
naturals = take 7 $ Cycle.toList $ Cycle.find fifth pcF

isNatural :: (Pitch' p) => p -> Bool
isNatural p = pitchClassOf p `elem` naturals

naturalOfFlat :: (Pitch' p) => p -> p
naturalOfFlat p
    | isNatural p   = p
    | otherwise     = sharp p

naturalOfSharp :: (Pitch' p) => p -> p
naturalOfSharp p
    | isNatural p   = p
    | otherwise     = flat p

sharpCount :: Key -> Int
sharpCount (Key n) = n

keySignature :: Key -> [PitchClass]
keySignature key
    | n == 0   = []
    | n > 0    = take n $ Cycle.toList $ Cycle.find fifth pcF
    | n < 0    = take (-n) $ Cycle.toList $ Cycle.find fifth' pcB
    where
        n = sharpCount key

keyPitches :: Key -> [PitchClass]
keyPitches = take 7 . Cycle.toList . Cycle.find fifth . fifth' . pitchClassOf

inKey :: (Pitch' p) => p -> Key -> Bool
p `inKey` key = pitchClassOf p `elem` keyPitches key
