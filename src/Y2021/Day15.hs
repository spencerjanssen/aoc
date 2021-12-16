module Y2021.Day15 where

import Data.FileEmbed
import Data.Map.Lazy qualified as Map
import Data.OrdPSQ qualified as PSQ
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

riskMap :: [[Int]] -> Map (Int, Int) Risk
riskMap rss = fromList [((i, j), Risk r) | (i, rs) <- zip [0 ..] rss, (j, r) <- zip [0 ..] rs]

risksToGraph :: [[Int]] -> Graph (Int, Int) Risk
risksToGraph rs = fromPaths [(ij, ij', w) | ij <- keys rm, ij' <- neighbors ij, w <- maybeToList $ rm Map.!? ij']
  where
    rm = riskMap rs
    neighbors (i, j) =
        [ (i + di, j + dj)
        | di <- [-1 .. 1]
        , dj <- [-1 .. 1]
        , di /= 0 || dj /= 0
        , di == 0 || dj == 0
        ]

type Graph v w = Map v [(v, w)]

fromPaths :: Ord v => [(v, v, w)] -> Graph v w
fromPaths uvws = Map.fromListWith (<>) [(u, [(v, w)]) | (u, v, w) <- uvws]

adjacent :: Ord v => v -> Graph v w -> [(v, w)]
adjacent x g = fromMaybe [] $ lookup x g

shortestPath :: (Monoid wg, Ord v, Ord wg) => v -> v -> Graph v wg -> Maybe wg
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

-- >>> part1 parsedExample
-- Just (Risk {getRisk = 40})
-- >>> part1 parsedProblem
-- Just (Risk {getRisk = 388})
part1 :: [[Int]] -> Maybe Risk
part1 rss = do
    rs0 <- nonEmpty rss
    shortestPath (0, 0) (length rss - 1, length rs0 - 1) (risksToGraph rss)

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= Just (Risk 40)

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= Just (Risk 388)
