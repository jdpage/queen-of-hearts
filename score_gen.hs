import Data.Cycle (Cycle)
import qualified Data.Cycle as Cycle
import Data.List
import Numeric.GaussianRing
import System.Environment
import Text.Printf

data Sentence = Sentence Int Float deriving (Eq)

instance Show Sentence where
    show (Sentence l p) = printf "%d %f" l p

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

mapLength :: Zni Integer -> Int
mapLength = floor . magnitude

mapPassion :: Zni Integer -> Float
mapPassion z = mp' (argument z) where
    mp' i
        | 0 <= i && i <= (pi / 2)   = 2 * i / pi
        | otherwise                 = error $ "out of range " ++ show i

mapSentence :: Zni Integer -> Sentence
mapSentence z = Sentence (mapLength z) (mapPassion z)

mapCycle :: Cycle (Zni Integer) -> [Sentence]
mapCycle = map mapSentence . Cycle.toList1

main :: IO ()
main = do
    args <- getArgs
    let cs = enumerateAllCycles $ read $ (args !! 0)
    let lim = read $ args !! 1
    mapM_ putStrLn $ do
        c <- cs
        if Cycle.length c < lim then [] else do
            let cc = constantOf c
            let ss = map show $ mapCycle c
            return $ (show cc) ++ " " ++ (unwords ss)
