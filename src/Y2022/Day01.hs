module Y2022.Day01 where

import Data.FileEmbed (embedFile)
import Data.List.NonEmpty (some1)
import MegaParsecUtil
import Relude.Extra (Foldable1 (maximum1))
import Test.Tasty.HUnit
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

example :: Text
example = decodeUtf8 $(embedFile "inputs/2022/day01/example.txt")

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2022/day01/problem.txt")

-- >>> parsedExample
-- [[1000,2000,3000],[4000],[5000,6000],[7000,8000,9000],[10000]]
parsedExample :: NonEmpty (NonEmpty Int)
parsedExample = parseThrow elves "example" example

parsedProblem :: NonEmpty (NonEmpty Int)
parsedProblem = parseThrow elves "problem" problem

elves :: Parsec Void Text (NonEmpty (NonEmpty Int))
elves = some1 (elf <* (void eol <|> eof))

elf :: Parsec Void Text (NonEmpty Int)
elf = some1 (int <* eol)

-- >>> part1 $ (1 :| [2]) :| [pure 1, pure 2]
-- 3
part1 :: NonEmpty (NonEmpty Int) -> Int
part1 = maximum1 . fmap sum

-- >>> part2 parsedExample
-- 45000
part2 :: NonEmpty (NonEmpty Int) -> Int
part2 = sum . take 3 . sortBy (flip compare) . toList . fmap sum

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 24000

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 68292

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= 45000

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= 203203
