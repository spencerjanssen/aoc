{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2022.Day08 where

import AocUtil
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty
import Test.Tasty.SmallCheck
import Text.Megaparsec hiding (some)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char

puzzle :: Puzzle [[Int]]
puzzle =
    Puzzle
        { day = "08"
        , year = "2022"
        , parser = grid
        , parts = [Part part1 21 1690, Part part2 8 535680]
        }

test_ :: TestTree
test_ = tests puzzle

grid :: Parsec Void Text [[Int]]
grid = endBy (MP.many digit) eol <* eof

data Rotation = Rotation
    { into :: forall a. [[a]] -> [[a]]
    , outof :: forall a. [[a]] -> [[a]]
    }

instance Semigroup Rotation where
    Rotation f fi <> Rotation g gi = Rotation (f . g) (gi . fi)

instance Monoid Rotation where
    mempty = Rotation id id

selfInverse :: (forall a. [[a]] -> [[a]]) -> Rotation
selfInverse f = Rotation f f

rotations :: [Rotation]
rotations = liftA2 (<>) [mempty, selfInverse $ map reverse] [mempty, selfInverse transpose]

underRotation :: ([[a]] -> [[b]]) -> Rotation -> ([[a]] -> [[b]])
underRotation f (Rotation r ri) xs = ri $ f $ r xs

gridly :: forall a. (Monoid a) => ([Int] -> [a]) -> [[Int]] -> Ap ZipList (Ap ZipList a)
gridly f g = foldMap (\r -> coerce @_ @(Ap ZipList (Ap ZipList a)) $ underRotation (map f) r g) rotations

solve :: (Monoid c, Monoid a) => ([Int] -> [a]) -> (a -> c) -> [[Int]] -> c
solve f g = foldMap (foldMap g) . gridly f

part1 :: [[Int]] -> Int
part1 = getSum . solve visibility (\(Any b) -> if b then 1 else 0)

part2 :: [[Int]] -> Int
part2 = coerce . solve viewDistances (\(Product p) -> Max p)

visibility :: [Int] -> [Any]
visibility xs = zipWith ((Any .) . (>)) mxs heights
  where
    mxs = map (Just . Max) xs
    heights = scanl (<>) Nothing mxs

viewDistance :: [Int] -> Int -> Int
viewDistance hs h = case span (h >) hs of
    (l, r) -> length l + length (take 1 r)

-- >>> viewDistances [2, 5, 5, 1, 2]
-- [0,1,1,1,2]
viewDistances :: [Int] -> [Product Int]
viewDistances xs = coerce $ zipWith viewDistance horizons xs
  where
    horizons = scanl (flip (:)) [] xs

scprop_rotations_inverse :: [[Bool]] -> Property IO
scprop_rotations_inverse xs = ([] `notElem` xs && sameLength) ==> all (\r -> xs == underRotation id r xs) rotations
  where
    sameLength = case xs of
        [] -> True
        (y : ys) -> all ((length y ==) . length) ys
