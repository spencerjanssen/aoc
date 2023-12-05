module Y2021.Day04 where

import Data.FileEmbed (embedFile)
import Data.IntSet qualified as IntSet
import MegaParsecUtil
import Test.Tasty.HUnit
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day04/example.txt")

parsedExample :: ([Int], [[[Int]]])
parsedExample = parseThrow parser "" example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day04/problem.txt")

parsedProblem :: ([Int], [[[Int]]])
parsedProblem = parseThrow parser "" problem

-- >>> parseThrow (moves <* eof) "" "1234,5"
-- [1234,5]
moves :: Parsec Void Text [Int]
moves = sepBy1 int (string ",")

-- >>> parseThrow (line <* eof) "" " 1 12  5 10"
-- [1,12,5,10]
line :: Parsec Void Text [Int]
line = do
    skipMany " "
    l <- sepBy1 int (skipSome $ string " ")
    void eol
    pure l

-- >>> parseThrow (board <* eof) "" " 1 12  5 10\n1 2 3 4\n"
-- [[1,12,5,10],[1,2,3,4]]
board :: Parsec Void Text [[Int]]
board = some line

-- >>> parseThrow parser "" example
-- ([7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],[[[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]],[[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],[[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]]])
parser :: Parsec Void Text ([Int], [[[Int]]])
parser = do
    ms <- moves
    skipSome $ void eol
    bs <- sepBy1 board (skipSome $ void eol)
    eof
    pure (ms, bs)

-- >>> buildMoves [1, 2, 3]
-- [(1,fromList [1]),(2,fromList [1,2]),(3,fromList [1,2,3])]
buildMoves :: [Int] -> [(Int, IntSet)]
buildMoves = drop 1 . scanl (\(_, s) i -> (i, IntSet.insert i s)) (error "this is unreachable", mempty)

isBingo :: IntSet -> [[Int]] -> Bool
isBingo s b = any (all (`IntSet.member` s)) $ b <> transpose b

playBingo :: [(Int, IntSet)] -> [[Int]] -> Maybe (Int, IntSet)
playBingo ms b = find ((`isBingo` b) . snd) ms

score :: ([[Int]], (Int, IntSet)) -> Int
score (b, (w, ws)) = w * sum (filter (not . (`IntSet.member` ws)) $ concat b)

boardWins :: [Int] -> [[[Int]]] -> [([[Int]], (Int, IntSet))]
boardWins ms = mapMaybe (\b -> (b,) <$> playBingo bms b)
  where
    bms = buildMoves ms

-- this is in relude 1.0.0.0
minimumOn1 :: (Ord b) => (a -> b) -> [a] -> Maybe a
minimumOn1 f = listToMaybe . sortOn f

maximumOn1 :: (Ord b) => (a -> b) -> [a] -> Maybe a
maximumOn1 f = coerce . listToMaybe . sortOn (Down . f)

-- >>> part1 parsedExample
-- 4512
-- >>> part1 parsedProblem
-- 2745
part1 :: ([Int], [[[Int]]]) -> Maybe Int
part1 = fmap score . minimumOn1 (IntSet.size . snd . snd) . uncurry boardWins

-- >>> part2 parsedExample
-- Just 1924
-- >>> part2 parsedProblem
-- Just 6594
part2 :: ([Int], [[[Int]]]) -> Maybe Int
part2 = fmap score . maximumOn1 (IntSet.size . snd . snd) . uncurry boardWins

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= Just 4512

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= Just 2745

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= Just 1924

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= Just 6594
