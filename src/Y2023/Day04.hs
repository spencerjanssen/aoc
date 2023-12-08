module Y2023.Day04 where

import AocUtil
import Data.IntSet qualified as Set
import Data.Map.Lazy qualified as Map
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (First (..), Last (..), many, round, some)

puzzle :: Puzzle [Card]
puzzle =
    Puzzle
        { year = "2023"
        , day = "04"
        , parser = cards
        , parts =
            [ Part part1 13 20829
            , Part part2 30 12648035
            ]
        }

test_ :: TestTree
test_ = tests puzzle

data Card = Card
    { cardNo :: Int
    , winning :: [Int]
    , have :: [Int]
    }

cards :: Parsec Void Text [Card]
cards = endBy card eol <* eof

card :: Parsec Void Text Card
card = do
    void "Card"
    void $ some sp
    cardNo <- int
    void ": "
    void $ many sp
    winning <- endBy int (some sp)
    void "| "
    void $ many sp
    have <- sepBy int (some sp)
    pure Card{cardNo, winning, have}
  where
    sp = char ' '

part1 :: [Card] -> Int
part1 = sum . map (score . winners)
  where
    score 0 = 0
    score n = 2 ^ pred n

winners :: Card -> Int
winners c = Set.size $ Set.intersection (Set.fromList c.winning) (Set.fromList c.have)

part2 :: [Card] -> Int
part2 cs = sum $ Map.elems m
  where
    f c = 1 + sum (map f' [succ c.cardNo .. c.cardNo + winners c])
    m = Map.fromList $ map (\c -> (c.cardNo, f c)) cs
    f' = fromMaybe 0 . flip Map.lookup m
