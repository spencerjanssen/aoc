module Y2021.Day7 where

import Data.FileEmbed
import Data.Text qualified as T
import Relude.Extra
import Test.Tasty.HUnit

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day7/day7.example.txt")

-- >>> parsedExample
-- [16,1,2,0,4,2,7,1,2,14]
parsedExample :: [Int]
parsedExample = parse example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day7/day7.problem.txt")

parsedProblem :: [Int]
parsedProblem = parse problem

type Position = Int
type Fuel = Sum Int

parse :: Text -> [Position]
parse = mapMaybe (readMaybe @Int . toString) . T.splitOn ","

fuelTo :: Position -> Position -> Fuel
fuelTo x y = Sum $ abs $ x - y

-- >>> part1 parsedExample
-- Just (Sum {getSum = 37})
-- >>> part1 parsedProblem
-- Just (Sum {getSum = 345035})
part1 :: [Int] -> Maybe Fuel
part1 (nonEmpty -> Just ps) = fmap snd $ listToMaybe $ sortOn snd $ [(p, foldMap (fuelTo p) ps) | p <- [minimum1 ps .. maximum1 ps]]
part1 _ = Nothing

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= Just 37

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= Just 345035
