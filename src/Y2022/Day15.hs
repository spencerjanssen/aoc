module Y2022.Day15 where

import AocUtil
import MegaParsecUtil
import Test.Tasty
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
beaconless y (ps@(sx, sy), pb@(bx, by))
    | dy > d = Nothing
    | y == by = excludeExtreme bx i
    | otherwise = Just i
  where
    d = manhattan ps pb
    dy = abs (sy - y)
    i = (sx - d + dy, sx + d - dy)

excludeExtreme :: Int -> Interval -> Maybe Interval
excludeExtreme p (x, y)
    | p == x && p == y = Nothing
    | p == x = Just (succ x, y)
    | p == y = Just (x, pred y)
    | otherwise = Just (x, y)

type IntervalSet = [(Int, Int)]

inInterval :: Int -> Interval -> Bool
inInterval n (u, v) = n >= u && n <= v

union :: Interval -> Interval -> Either (Interval, Interval) Interval
union x@(x0, x1) y@(y0, y1)
    | inInterval x0 y || inInterval y0 x = Right (min x0 y0, max x1 y1)
    | otherwise = Left (x, y)

magnitude :: Interval -> Sum Int
magnitude (x, y) = Sum $ y - x + 1

intervalSet :: [Interval] -> [Interval]
intervalSet [] = []
intervalSet (x : xs) = case partitionWith (union x) xs of
    (_, []) -> x : intervalSet xs
    (ns, o : os) -> intervalSet $ (o : os) <> map snd ns

part1 :: Int -> [(Point, Point)] -> Sum Int
part1 y = foldMap magnitude . intervalSet . intervalSet . mapMaybe (beaconless y)
