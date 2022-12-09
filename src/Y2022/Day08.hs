{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2022.Day08 where

import Data.FileEmbed (embedFile)
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Text.Megaparsec hiding (some)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char

example :: Text
example = decodeUtf8 $(embedFile "inputs/2022/day08/example.txt")

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2022/day08/problem.txt")

grid :: Parsec Void Text [[Int]]
grid = endBy (MP.many digit) eol <* eof

-- >>> parsedExample
-- [[3,0,3,7,3],[2,5,5,1,2],[6,5,3,3,2],[3,3,5,4,9],[3,5,3,9,0]]
parsedExample :: [[Int]]
parsedExample = parseThrow grid "example" example

parsedProblem :: [[Int]]
parsedProblem = parseThrow grid "problem" problem

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

gridly :: forall a. Monoid a => ([Int] -> [a]) -> [[Int]] -> Ap ZipList (Ap ZipList a)
gridly f g = foldMap (\r -> coerce @_ @(Ap ZipList (Ap ZipList a)) $ underRotation (map f) r g) rotations

solve :: (Monoid c, Monoid a) => ([Int] -> [a]) -> (a -> c) -> [[Int]] -> c
solve f g = foldMap (foldMap g) . gridly f

part1 :: [[Int]] -> Int
part1 = getSum . solve visibility (\(Any b) -> if b then 1 else 0)

part2 :: [[Int]] -> Max Int
part2 = solve viewDistances (\(Product p) -> Max p)

-- >>> visibility . head <$> nonEmpty parsedExample
-- Just [Any {getAny = True},Any {getAny = False},Any {getAny = False},Any {getAny = True},Any {getAny = False}]
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

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 21

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 1690

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= 8

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= 535680

scprop_rotations_inverse :: [[Bool]] -> Property IO
scprop_rotations_inverse xs = ([] `notElem` xs && sameLength) ==> all (\r -> xs == underRotation id r xs) rotations
  where
    sameLength = case xs of
        [] -> True
        (y : ys) -> all ((length y ==) . length) ys
