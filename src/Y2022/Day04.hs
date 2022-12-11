module Y2022.Day04 where

import AocUtil
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

puzzle :: Puzzle [(Interval, Interval)]
puzzle =
    Puzzle
        { year = "2022"
        , day = "04"
        , parser = pairs
        , parts = [Part part1 2 511, Part part2 4 821]
        }

test_ :: TestTree
test_ = tests puzzle

data Interval = Interval Int Int

pairs :: Parsec Void Text [(Interval, Interval)]
pairs = endBy pair eol <* eof

pair :: Parsec Void Text (Interval, Interval)
pair = (,) <$> interval <* "," <*> interval

interval :: Parsec Void Text Interval
interval = Interval <$> int <* "-" <*> int

subset :: Interval -> Interval -> Bool
subset (Interval m n) (Interval o p) = m >= o && n <= p

contains :: Int -> Interval -> Bool
contains x (Interval u0 u1) = x >= u0 && x <= u1

overlaps :: Interval -> Interval -> Bool
overlaps u@(Interval u0 _) v@(Interval v0 _) = contains u0 v || contains v0 u

part1 :: [(Interval, Interval)] -> Int
part1 ps = length [(u, v) | (u, v) <- ps, subset u v || subset v u]

part2 :: [(Interval, Interval)] -> Int
part2 = length . filter (uncurry overlaps)
