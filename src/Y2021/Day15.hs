module Y2021.Day15 where

import Data.FileEmbed
import Data.HashPSQ qualified as PSQ
import Data.Set qualified as Set
import Relude.Extra.Map
import Test.Tasty.HUnit

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day15/day15.example.txt")

parsedExample :: [[Int]]
parsedExample = parse example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day15/day15.problem.txt")

parsedProblem :: [[Int]]
parsedProblem = parse problem

newtype Risk = Risk {getRisk :: Int}
    deriving (Show, Eq, Ord)
    deriving (Semigroup, Monoid) via (Sum Int)

parse :: Text -> [[Int]]
parse = fromMaybe (error "invalid input") . traverse (traverse (readMaybe . pure) . toString) . lines

advance :: Int -> Int
advance 9 = 1
advance i = succ i

tile :: Int -> [[Int]] -> [[Int]]
tile n = concat . take n . iterate adv1 . topRow
  where
    adv1 = map (map advance)
    topRow = foldr (zipWith (<>)) (repeat []) . take n . iterate adv1

riskMap :: [[Int]] -> Map (Int, Int) Risk
riskMap rss = fromList [((i, j), Risk r) | (i, rs) <- zip [0 ..] rss, (j, r) <- zip [0 ..] rs]

risksToGraph :: [[Int]] -> Graph (Int, Int) Risk
risksToGraph rs = fromAdjacencyList [(ij, [(ij', w) | ij' <- neighbors ij, w <- maybeToList $ rm !? ij']) | ij <- keys rm]
  where
    rm = riskMap rs
    dijs =
        [ (di, dj)
        | di <- [-1 .. 1]
        , dj <- [-1 .. 1]
        , (di == 0) /= (dj == 0)
        ]
    neighbors (i, j) =
        [ (i + di, j + dj)
        | (di, dj) <- dijs
        ]

type Graph v w = Map v [(v, w)]

fromAdjacencyList :: Ord v => [(v, [(v, w)])] -> Graph v w
fromAdjacencyList = fromList

adjacent :: Ord v => v -> Graph v w -> [(v, w)]
adjacent x g = maybe [] toList $ lookup x g

shortestPath :: (Monoid wg, Ord v, Ord wg, Hashable v) => v -> v -> Graph v wg -> Maybe wg
shortestPath start end g = go mempty (PSQ.singleton start mempty ())
  where
    decrease w' Nothing = ((), Just (w', ()))
    decrease w' (Just (w, ()))
        | w' < w = ((), Just (w', ()))
        | otherwise = ((), Just (w, ()))
    go completed (PSQ.minView -> (Just (i, w, (), weights)))
        | i == end = Just w
        | otherwise =
            let weights' =
                    foldl'
                        (\ws (j, w') -> snd $ PSQ.alter (decrease (w <> w')) j ws)
                        weights
                        [ (j, w')
                        | (j, w') <- adjacent i g
                        , not $ Set.member j completed
                        ]
                completed' = Set.insert i completed
             in go completed' weights'
    go _ _ = Nothing

solve :: [[Int]] -> Maybe Risk
solve rss = do
    rs0 <- nonEmpty rss
    shortestPath (0, 0) (length rss - 1, length rs0 - 1) (risksToGraph rss)

-- >>> part1 parsedExample
-- Just (Risk {getRisk = 40})
-- >>> part1 parsedProblem
-- Just (Risk {getRisk = 388})
part1 :: [[Int]] -> Maybe Risk
part1 = solve

-- >>> part2 parsedExample
-- Just (Risk {getRisk = 315})
-- >>> part2 parsedProblem
-- Just (Risk {getRisk = 2819})
part2 :: [[Int]] -> Maybe Risk
part2 = solve . tile 5

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= Just (Risk 40)

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= Just (Risk 388)

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= Just (Risk 315)

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= Just (Risk 2819)
