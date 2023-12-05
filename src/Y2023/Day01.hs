module Y2023.Day01 where

import AocUtil
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (First (..), Last (..), many, round, some)

puzzle :: Puzzle [[Either Char Int]]
puzzle =
    Puzzle
        { year = "2023"
        , day = "01"
        , parser = block
        , parts =
            [ Part part1 (Just 142) (Just 54940)
            , Part part2 (Just 142) (Just 54208)
            ]
        }

test_ :: TestTree
test_ = tests puzzle

part2Example :: Text
part2Example =
    unlines
        [ "two1nine"
        , "eightwothree"
        , "abcone2threexyz"
        , "xtwone3four"
        , "4nineeightseven2"
        , "zoneight234"
        , "7pqrstsixteen"
        ]

test_parser :: TestTree
test_parser =
    testGroup
        "Year 2023 day 01"
        [ testCase "Part 2 example" $ do
            p2 <- parseThrowIO block "part2Example" part2Example
            part2 p2 @?= Just 281
        ]

solve :: [[Int]] -> Maybe Int
solve = fmap getSum . foldMap sline
  where
    sline =
        foldMap (\d -> Just (First d, Last d))
            >>> fmap \(First mf, Last ml) -> Sum (mf * 10 + ml)

part1 :: [[Either Char Int]] -> Maybe Int
part1 = solve . map (mapMaybe (either (const Nothing) Just))

part2 :: [[Either Char Int]] -> Maybe Int
part2 = solve . map postprocess

postprocess :: [Either Char Int] -> [Int]
postprocess [] = []
postprocess (Right n : xs) = n : postprocess xs
postprocess (x : xs) =
    [ d
    | (w, d) <- wordDigits
    , let w' = map Left $ toString w
    , w' `isPrefixOf` (x : xs)
    ]
        <> postprocess xs

wordDigits :: [(Text, Int)]
wordDigits =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

block :: Parsec Void Text [[Either Char Int]]
block = endBy (many tok) eol <* eof

tok :: Parsec Void Text (Either Char Int)
tok = (Left <$> asciiAlphaChar) <|> (Right <$> digit)
