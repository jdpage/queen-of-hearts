module Data.Cycle
    ( Cycle()
    , unfold
    , find
    , length
    , head
    , tail
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

length :: Cycle a -> Int
length (Cycle zs) = List.length zs

instance (Eq a) => Eq (Cycle a) where
    (Cycle zs) == (Cycle ws)   = cycleEq zs ws where
        cycleEq zs ws =
            let zl = List.length zs in (zl == List.length ws) && (cycleEq' zl zs ws)
            where
                cycleEq' 0 _ _ = False
                cycleEq' n zs (w:ws) = (zs == (w:ws)) || (cycleEq' (n - 1) zs (ws ++ [w]))

head :: Cycle a -> a
head (Cycle []) = error "zero-length cycle"
head (Cycle (z:_)) = z

tail :: Cycle a -> Cycle a
tail (Cycle []) = error "zero-length cycle"
tail (Cycle (z:zs)) = Cycle $ zs ++ [z]

toList :: Cycle a -> [a]
toList (Cycle zs) = cycle zs

toList1 :: Cycle a -> [a]
toList1 (Cycle zs) = zs

