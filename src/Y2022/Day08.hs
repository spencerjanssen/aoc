{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2022.Day08 where

import Data.FileEmbed (embedFile)
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty.HUnit
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

newtype Rotation = Rotation (forall a. [[a]] -> [[a]])

composeRotation :: Rotation -> Rotation -> Rotation
composeRotation (Rotation f) (Rotation g) = Rotation (f . g)

runRotation :: Rotation -> [[a]] -> [[a]]
runRotation (Rotation f) = f

-- >>> map ((`runRotation` [[1, 2, 3], [4, 5, 6]]) . fst) rotations
-- [[[1,2,3],[4,5,6]],[[1,4],[2,5],[3,6]],[[3,2,1],[6,5,4]],[[4,1],[5,2],[6,3]]]
rotations :: [(Rotation, Rotation)]
rotations =
    liftA2
        (\f g -> (composeRotation f g, composeRotation g f))
        [Rotation id, Rotation $ map reverse]
        [Rotation id, Rotation transpose]

gridly :: forall a. Monoid a => ([Int] -> [a]) -> [[Int]] -> Ap ZipList (Ap ZipList a)
gridly f g =
    mconcat
        [ as
        | (into, outof) <- rotations
        , let g' = runRotation into g
        , let as = coerce @_ @(Ap ZipList (Ap ZipList a)) $ runRotation outof $ map f g'
        ]

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
