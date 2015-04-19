module Music
    ( Pitch(..)
    , PitchClass()
    , Key()
    , Accidental(..)
    , Octave(..)
    , pcC, pcCis, pcD, pcDis, pcE, pcF, pcFis, pcG, pcGis, pcA, pcAis, pcB
    , pcAes, pcBes, pcDes, pcEes, pcGes
    , keyGes, keyDes, keyAes, keyEes, keyBes, keyF, keyC, keyG, keyD, keyA
    , keyE, keyB, keyFis
    , keyEesm, keyBesm, keyFm, keyCm, keyGm, keyDm, keyAm, keyEm, keyBm
    , keyFism, keyCism, keyGism, keyDism 
    , pitchClassOf, octaveOf
    , pitch
    , sharp, flat, sharps, flats
    , fifthCircle
    , sharpCount
    ) where

data Pitch = Pitch Int deriving (Eq) -- midi number
data PitchClass = PitchClass Int deriving (Eq)
data Key = Key Int deriving (Eq)
data Accidental = Sharp | Natural | Flat deriving (Eq, Show)
data KeyInfo = KeyInfo Accidental Accidental Accidental Accidental Accidental Accidental Accidental Accidental Accidental Accidental Accidental Accidental

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

pitchClassOf :: Pitch -> PitchClass
pitchClassOf (Pitch n) = PitchClass $ n `mod` 12

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

instance Pitch' PitchClass where
    sharps k (PitchClass n) = PitchClass $ (n + k) `mod` 12

instance Pitch' Pitch where
    sharps k (Pitch n) = Pitch $ n + k

fifthCircle :: [PitchClass]
fifthCircle = [PitchClass $ x * 7 `mod` 12 | x <- [0..11]]

sharpCount :: Key -> Int
sharpCount (Key n) = n

