module Y2021.Day05 where

import Data.FileEmbed
import MegaParsecUtil
import Relude.Extra.Map
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (some)

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day05/example.txt")

-- >>> parsedExample
-- [((0,9),(5,9)),((8,0),(0,8)),((9,4),(3,4)),((2,2),(2,1)),((7,0),(7,4)),((6,4),(2,0)),((0,9),(2,9)),((3,4),(1,4)),((0,0),(8,8)),((5,5),(8,2))]
parsedExample :: [((Int, Int), (Int, Int))]
parsedExample = parseThrow segments "example" example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day05/problem.txt")

parsedProblem :: [((Int, Int), (Int, Int))]
parsedProblem = parseThrow segments "problem" problem

type Coord = (Int, Int)
type Segment = (Coord, Coord)
type Placements = Map Coord Int
type ExpandedSegment = [Coord]

-- >>> parseThrow coord "" "11,97"
-- (11,97)
coord :: Parsec Void Text Coord
coord = do
    x <- int
    void $ string ","
    y <- int
    pure (x, y)

-- >>> parseThrow segment "" "3,4 -> 1,4"
-- ((3,4),(1,4))
segment :: Parsec Void Text Segment
segment = do
    s <- coord
    void $ string " -> "
    e <- coord
    void eol
    pure (s, e)

segments :: Parsec Void Text [Segment]
segments = do
    segs <- some segment
    eof
    pure segs

data SegmentMode
    = NonDiagonal
    | IncludeDiagonal
    deriving (Eq)

-- >>> expandSegment NonDiagonal ((0, 0), (0, 3))
-- Just [(0,0),(0,1),(0,2),(0,3)]
-- >>> expandSegment IncludeDiagonal ((0, 0), (3, 3))
-- Just [(0,0),(1,1),(2,2),(3,3)]
-- >>> expandSegment IncludeDiagonal ((5, 3), (2, 0))
-- Just [(5,3),(4,2),(3,1),(2,0)]
-- >>> expandSegment IncludeDiagonal ((9, 7), (7, 9))
-- Just [(9,7),(8,8),(7,9)]
expandSegment :: SegmentMode -> Segment -> Maybe ExpandedSegment
expandSegment sm ((x0, y0), (x1, y1))
    | x0 == x1 = Just [(x0, y) | y <- [min y0 y1 .. max y0 y1]]
    | y0 == y1 = Just [(x, y0) | x <- [min x0 x1 .. max x0 x1]]
    | sm == IncludeDiagonal && abs dx == abs dy =
        Just $
            zip
                [x0, x0 + signum dx .. x1]
                [y0, y0 + signum dy .. y1]
    | otherwise = Nothing
  where
    dx = x1 - x0
    dy = y1 - y0

placeCoord :: Coord -> Placements -> Placements
placeCoord c = insertWith (+) c 1

occupied :: SegmentMode -> [Segment] -> Int
occupied sm =
    length
        . filter (> 1)
        . elems
        . foldl' (flip placeCoord) mempty
        . concat
        . mapMaybe (expandSegment sm)

-- >>> part1 parsedExample
-- 5
-- >>> part1 parsedProblem
-- 7380
part1 :: [Segment] -> Int
part1 = occupied NonDiagonal

-- >>> part2 parsedExample
-- 12
-- >>> part2 parsedProblem
-- 21373
part2 :: [Segment] -> Int
part2 = occupied IncludeDiagonal

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 5

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 7380

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= 12

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= 21373
