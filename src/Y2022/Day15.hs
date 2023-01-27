module Y2022.Day15 where

import AocUtil
import MegaParsecUtil
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle [(Point, Point)]
puzzle =
    Puzzle
        { year = "2022"
        , day = "15"
        , parser = sensors
        , parts = [Part (part1 2_000_000) 0 5_299_855]
        }

test_ :: TestTree
test_ = tests puzzle

test_part2 :: TestTree
test_part2 =
    testGroup
        "Part 2"
        [ testCase "Example" $ do
            i <- puzzleInput puzzle Example
            map tuningFrequency (search (0, 20) i) @?= [56_000_011]
        , testCase "Problem" $ do
            i <- puzzleInput puzzle Problem
            map tuningFrequency (search (0, 4_000_000) i) @?= [13615843289729]
        ]

type Point = (Int, Int)

sensors :: ParsecT Void Text Identity [(Point, Point)]
sensors = endBy sensor eol <* eof

sensor :: ParsecT Void Text Identity (Point, Point)
sensor = (,) <$> ("Sensor at " *> coord) <*> (": closest beacon is at " *> coord)

coord :: Parsec Void Text Point
coord = (,) <$> ("x=" *> signedInt) <*> (", y=" *> signedInt)

type Interval = (Int, Int)

-- >>> manhattan (8, 7) (2, 10)
-- 9
manhattan :: Point -> Point -> Int
manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

-- >>> beaconless 10 ((8, 7), (2, 10))
-- Just (3,14)
beaconless :: Int -> (Point, Point) -> Maybe Interval
beaconless y (ps@(sx, sy), pb)
    | dy > d = Nothing
    | otherwise = Just i
  where
    d = manhattan ps pb
    dy = abs (sy - y)
    i = (sx - d + dy, sx + d - dy)

type IntervalSet = [(Int, Int)]

inInterval :: Int -> Interval -> Bool
inInterval n (u, v) = n >= u && n <= v

union :: Interval -> Interval -> Maybe Interval
union x@(x0, x1) y@(y0, y1)
    | or [succ x1 == y0, succ y1 == x0, inInterval x0 y, inInterval y0 x] = Just (min x0 y0, max x1 y1)
    | otherwise = Nothing

magnitude :: Interval -> Sum Int
magnitude (x, y) = Sum $ y - x + 1

intervalSet :: [Interval] -> [Interval]
intervalSet = foldl' (flip insert) []
  where
    insert x [] = [x]
    insert x (y : ys) = case x `union` y of
        Just z -> insert z ys
        Nothing
            | x > y -> y : insert x ys
            | otherwise -> x : y : ys

noBeacons :: Int -> [(Point, Point)] -> [Interval]
noBeacons y = intervalSet . mapMaybe (beaconless y)

part1 :: Int -> [(Point, Point)] -> Sum Int
part1 y ps = foldMap magnitude (noBeacons y ps) <> Sum (negate $ length $ ordNub $ filter ((== y) . snd) $ map snd ps)

search :: Interval -> [(Point, Point)] -> [(Int, Int)]
search (lo, hi) ps =
    ordNub $
        [ (x, y)
        | y <- [lo .. hi]
        , (x0, x1) <- intervalSet $ mapMaybe (beaconless y) ps
        , x <- [pred x0, succ x1]
        , x > lo && x < hi
        ]

tuningFrequency :: Point -> Int
tuningFrequency (x, y) = x * 4_000_000 + y
