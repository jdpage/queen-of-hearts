module Data.Cycle
    ( Cycle()
    , unfold
    , find
    , prefix
    , length
    , toList, toList1
    ) where

import Prelude hiding (length, head, tail)
import qualified Data.List as List (length, head, tail)

data Cycle a = Cycle [a] deriving (Show)

unfold :: (a -> a) -> a -> [a]
unfold f z = z:(unfold f $ f z)

find :: (Eq a) => (a -> a) -> a -> Cycle a
find f z = Cycle (find' (unfold f z) [] []) where
    find' (z:zs) ms1 ms2
        | z `elem` ms2   = []
        | z `elem` ms1   = z : (find' zs ms1 (z:ms2))
        | otherwise      = find' zs (z:ms1) ms2

prefix :: (Eq a) => (a -> a) -> a -> [a]
prefix f z =
    let c = toList1 $ find f z in
    takeWhile (\x -> not $ x `elem` c) $ unfold f z

length :: Cycle a -> Int
length (Cycle zs) = List.length zs

instance (Eq a) => Eq (Cycle a) where
    (Cycle zs) == (Cycle ws)   = cycleEq zs ws where
        cycleEq zs ws =
            let zl = List.length zs in (zl == List.length ws) && (cycleEq' zl zs ws)
            where
                cycleEq' 0 _ _ = False
                cycleEq' n zs (w:ws) = (zs == (w:ws)) || (cycleEq' (n - 1) zs (ws ++ [w]))

toList :: Cycle a -> [a]
toList (Cycle zs) = cycle zs

toList1 :: Cycle a -> [a]
toList1 (Cycle zs) = zs

