import Numeric.GaussianRing
import Data.Cycle (Cycle)
import qualified Data.Cycle as Cycle
import Music

import Data.List
import System.Environment
import System.IO

data Note = Note Pitch Float

juliaOf :: (Ring x r) => r -> r -> r
juliaOf c z = (z *: z) +: c

cycleFrom :: (Ring x r) => r -> r -> Cycle r
cycleFrom c zi = Cycle.find (juliaOf c) zi

enumerateCycles :: (Integral a) => Zni a -> [Cycle (Zni a)]
enumerateCycles c = nub [cycleFrom c z | z <- enumerateZni (modulus c)]

constantOf :: (Ring x r) => Cycle r -> r
constantOf zs = w -: (z *: z) where
    z = Cycle.head zs
    w = Cycle.head $ Cycle.tail zs

enumerateAllCycles :: (Integral a) => a -> [Cycle (Zni a)]
enumerateAllCycles n = concat [enumerateCycles c | c <- enumerateZni n]

stats :: Integer -> (Int, Int)
stats n =
    let cs = [Cycle.length c | c <- enumerateAllCycles n] in
    let m = maximum cs in
    (m, length $ elemIndices m cs)

prompt :: (Read a) => String -> IO a
prompt s = do
    putStr s
    hFlush stdout
    line <- getLine
    return $ read line

mapPitch :: Pitch -> Zni Integer -> Pitch
mapPitch (Pitch rel) z = Pitch $ rel + (floor $ magnitude z)

mapImportance :: Zni Integer -> Float
mapImportance z = 2 * argument z / pi

mapNote :: Pitch -> Zni Integer -> Note
mapNote rel z = Note (mapPitch rel z) (mapImportance z)

mapNote' = mapNote $ Pitch 60 -- c'

-- pickNote :: (RealFloat a) => a -> String
pickNote = note . floor
    where
        note 0 = "c'"
        note 1 = "cis'"
        note 2 = "d'"
        note 3 = "dis'"
        note 4 = "e'"
        note 5 = "f'"
        note 6 = "fis'"
        note 7 = "g'"
        note 8 = "gis'"
        note 9 = "a'"
        note 10 = "ais'"
        note 11 = "b'"
        note 12 = "c''"
        note 13 = "cis''"
        note 14 = "d''"
        note 15 = "dis''"
        note 16 = "e''"
        note 17 = "f''"
        note 18 = "fis''"
        note 19 = "g''"
        note 20 = "gis''"
        note 21 = "a''"
        note 22 = "ais''"
        note 23 = "b''"
        note 24 = "c'''"
        note _ = error "no such note"

showNote :: Zni Integer -> String
showNote = pickNote . magnitude

showPart :: IO () -> IO ()
showPart notes = do
    putStrLn "{"
    notes
    putStrLn "}"

showLine :: Cycle (Zni Integer) -> IO ()
showLine zs = showPart $ do
    putStrLn $ unwords [showNote z | z <- Cycle.toList1 zs]

showLines :: [Cycle (Zni Integer)] -> IO ()
showLines cs = foldl' (>>) (return ()) [showLine c | c <- cs]
    
main :: IO ()
main = do
    args <- getArgs
    let cs = enumerateAllCycles $ read $ (args !! 0)
    showLines [c | c <- cs, Cycle.length c >= 20]

