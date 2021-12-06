module Y2021.Day6 where

import Data.FileEmbed
import Data.IntMap qualified as IM
import Data.List ((!!))
import Data.Text qualified as T
import Relude.Extra (toPairs)
import Test.Tasty.HUnit

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day6/day6.example.txt")

-- >>> parsedExample
-- [3,4,3,1,2]
parsedExample :: [Int]
parsedExample = parse example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day6/day6.problem.txt")

parsedProblem :: [Int]
parsedProblem = parse problem

parse :: Text -> [Int]
parse = mapMaybe (readMaybe @Int . toString) . T.splitOn ","

type School = IntMap Int

school :: [Int] -> School
school ts = IM.fromListWith (+) $ zip ts $ repeat 1

advance :: Int -> [Int]
advance 0 = [6, 8]
advance i = [pred i]

-- >>> advanceSchool $ school parsedExample
-- fromList [(0,1),(1,1),(2,2),(3,1)]
advanceSchool :: School -> School
advanceSchool c = IM.fromListWith (+) [(t', n) | (t, n) <- toPairs c, t' <- advance t]

score :: School -> Int
score = getSum . foldMap Sum

ntimes :: Int -> (a -> a) -> a -> a
ntimes n f x = iterate f x !! n

solve :: Int -> [Int] -> Int
solve n = score . ntimes n advanceSchool . school

-- >>> part1 parsedExample
-- 5934
-- >>> part1 parsedProblem
-- 380243
part1 :: [Int] -> Int
part1 = solve 80

-- >>> part2 parsedExample
-- 26984457539
-- >>> part2 parsedProblem
-- 1708791884591
part2 :: [Int] -> Int
part2 = solve 256

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 5934

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 380243

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= 26984457539

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= 1708791884591
