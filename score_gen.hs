import Data.Cycle (Cycle)
import qualified Data.Cycle as Cycle
import Data.List
import Music
import Music.LilyPond
import Numeric.GaussianRing
import System.Environment
import System.IO

data Note = Note Pitch Float deriving (Show, Eq)

juliaOf :: (Ring x r) => r -> r -> r
juliaOf c z = (z *: z) +: c

cycleFrom :: (Ring x r) => r -> r -> Cycle r
cycleFrom c zi = Cycle.find (juliaOf c) zi

enumerateCycles :: (Integral a) => Zni a -> [Cycle (Zni a)]
enumerateCycles c = nub [cycleFrom c z | z <- enumerateZni (modulus c)]

constantOf :: (Ring x r) => Cycle r -> r
constantOf zs = let (z:w:_) = Cycle.toList zs in w -: (z *: z)

enumerateAllCycles :: (Integral a) => a -> [Cycle (Zni a)]
enumerateAllCycles n = enumerateZni n >>= enumerateCycles

mapPitch :: Pitch -> Zni Integer -> Pitch
mapPitch (Pitch rel) z = Pitch $ rel + (floor $ magnitude z)

mapImportance :: Zni Integer -> Float
mapImportance z = mi' (argument z) where
    mi' i
        | 0 <= i && i <= (pi / 2)   = 2 * i / pi
        | otherwise                 = error $ "out of range " ++ show i

mapNote :: Pitch -> Zni Integer -> Note
mapNote rel z = Note (mapPitch rel z) (mapImportance z)

mapNote' = mapNote $ Pitch 60 -- c'

mapCycle :: Cycle (Zni Integer) -> [Note]
mapCycle = map mapNote' . Cycle.toList1

-- assigns as grade to each key signature as follows:
-- two points for every accidental
-- one point for every sharp/flat in the key signature
gradeKeySignature :: Key -> [Note] -> Int
gradeKeySignature key notes = accidentals + (abs $ sharpCount key) where
    accidentals = sum [1 | (Note p _) <- notes, not $ p `inKey` key]

pickKeySignature :: [Note] -> Key
pickKeySignature ns =
    let (key, score) = foldl' f (keyC, worstScore) keys in key where
        worstScore = 2 * length ns + 7
        f (bestKey, bestScore) key
            | score < bestScore    = (key, score)
            | otherwise            = (bestKey, bestScore)
            where score = gradeKeySignature key ns

showNote :: Note -> Engraver String
showNote (Note p i) = do
    pp <- showPitch p
    let ii = show $ floor (i * 8)
    return $ pp ++ "-" ++ ii

showLine :: [Note] -> Engraver ()
showLine ns = do
    clef Treble
    key $ pickKeySignature ns
    emitLn $ "\\markup { test }"
    emitLn $ "\\time " ++ (show $ length ns) ++ "/4"
    emitLn "\\repeat volta 2 {"
    notes <- mapM showNote ns
    emitLn $ unwords [n | n <- notes]
    emitLn "}"
    emitLn $ "\\break"

main :: IO ()
main = do
    args <- getArgs
    let cs = enumerateAllCycles $ read $ (args !! 0)
    withFile (args !! 2) WriteMode $ engrave $ do
        version "2.18.2"
        part $ do
            mapM_ showLine [mapCycle c | c <- cs, Cycle.length c >= (read $ args !! 1)]
