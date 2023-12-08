module Y2023.Day04 where

import AocUtil
import Data.IntSet qualified as Set
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
part1 cs =
    sum
        [ score winners
        | c <- cs
        , let winners = Set.size $ Set.intersection (Set.fromList c.winning) (Set.fromList c.have)
        ]
  where
    score 0 = 0
    score n = 2 ^ pred n
