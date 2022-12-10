module Y2022.Day03 where

import AocUtil
import Data.Char
import Data.Set qualified as Set
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

puzzle :: Puzzle [[Char]]
puzzle =
    Puzzle
        { year = "2022"
        , day = "03"
        , parser = rucksack
        , parts = [Part part1 157 8088]
        }

test_ :: TestTree
test_ = tests puzzle

rucksack :: Parsec Void Text [[Char]]
rucksack = endBy (many asciiAlphaChar) eol <* eof

-- >>> compartments "abcd"
-- ("ab","cd")
compartments :: [Char] -> ([Char], [Char])
compartments xs = splitAt (length xs `div` 2) xs

score :: Char -> Int
score x
    | isAsciiLower x = 1 + ord x - ord 'a'
    | isAsciiUpper x = 27 + ord x - ord 'A'
    | otherwise = error $ show x <> " not in expected range."

part1 :: [[Char]] -> Int
part1 = getSum . foldMap part1Line

-- >>> map part1Line <$> puzzleInput puzzle Example
-- [Sum {getSum = 16},Sum {getSum = 12},Sum {getSum = 16},Sum {getSum = 22},Sum {getSum = 20},Sum {getSum = 19}]
part1Line :: [Char] -> Sum Int
part1Line x = common
  where
    (l, r) = compartments x
    common = foldMap (Sum . score) $ Set.intersection (fromList l) (fromList r)
