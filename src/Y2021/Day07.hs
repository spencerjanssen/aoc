module Y2021.Day07 where

import Data.FileEmbed
import Data.Text qualified as T
import Relude.Extra
import Test.Tasty.HUnit

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day07/example.txt")

-- >>> parsedExample
-- [16,1,2,0,4,2,7,1,2,14]
parsedExample :: [Int]
parsedExample = parse example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day07/problem.txt")

parsedProblem :: [Int]
parsedProblem = parse problem

type Position = Int
type Fuel = Sum Int

parse :: Text -> [Position]
parse = mapMaybe (readMaybe @Int . toString) . T.splitOn ","

fuelTo1 :: Position -> Position -> Fuel
fuelTo1 x y = Sum $ abs $ x - y

-- >>> fuelTo2 16 5
-- Sum {getSum = 66}
fuelTo2 :: Position -> Position -> Fuel
fuelTo2 x y = Sum $ n * (n + 1) `div` 2
  where
    n = abs $ x - y

solve :: (Int -> Int -> Fuel) -> [Int] -> Maybe Fuel
solve ft (nonEmpty -> Just ps) = fmap snd $ listToMaybe $ sortOn snd $ [(p, foldMap (ft p) ps) | p <- [minimum1 ps .. maximum1 ps]]
solve _ _ = Nothing

-- >>> part1 parsedExample
-- Just (Sum {getSum = 37})
-- >>> part1 parsedProblem
-- Just (Sum {getSum = 345035})
part1 :: [Int] -> Maybe Fuel
part1 = solve fuelTo1

-- >>> part2 parsedExample
-- Just (Sum {getSum = 168})
-- >>> part2 parsedProblem
-- Just (Sum {getSum = 97038163})
part2 :: [Int] -> Maybe Fuel
part2 = solve fuelTo2

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= Just 37

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= Just 345035

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= Just 168

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= Just 97038163
