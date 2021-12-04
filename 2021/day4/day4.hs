{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.FileEmbed (embedFile)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.List.Extra (minimumOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Text.Read (readMaybe)

example :: Text
example = T.decodeUtf8 $(embedFile "2021/day4/day4.example.txt")

parsedExample :: ([Int], [[[Int]]])
parsedExample = either (error . errorBundlePretty) id $ parse parser "" example

problem :: Text
problem = T.decodeUtf8 $(embedFile "2021/day4/day4.problem.txt")

parsedProblem :: ([Int], [[[Int]]])
parsedProblem = either (error . errorBundlePretty) id $ parse parser "" problem

int :: Parsec Void Text Int
int = maybe (fail "invalid number") pure . (readMaybe . T.unpack) =<< takeWhile1P (Just "digit") isDigit

-- >>> either (error . errorBundlePretty) id $ parse (moves <* eof) "" "1234,5"
-- [1234,5]
moves :: Parsec Void Text [Int]
moves = sepBy1 int (string ",")

-- >>> either (error . errorBundlePretty) id $ parse (line <* eof) "" " 1 12  5 10"
-- [1,12,5,10]
line :: Parsec Void Text [Int]
line = do
    skipMany " "
    l <- sepBy1 int (skipSome $ string " ")
    void eol
    pure l

-- >>> either (error . errorBundlePretty) id $ parse (board <* eof) "" " 1 12  5 10\n1 2 3 4\n"
-- [[1,12,5,10],[1,2,3,4]]
board :: Parsec Void Text [[Int]]
board = some line

-- >>> either (error . errorBundlePretty) id $ parse parser "" example
-- ([7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],[[[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]],[[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],[[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]]])
parser :: Parsec Void Text ([Int], [[[Int]]])
parser = do
    ms <- moves
    skipSome $ void eol
    bs <- sepBy1 board (skipSome $ void eol)
    eof
    pure (ms, bs)

-- >>> buildMoves [1, 2, 3]
-- [(1,fromList [1]),(2,fromList [1,2]),(3,fromList [1,2,3])]
buildMoves :: [Int] -> [(Int, IntSet)]
buildMoves = drop 1 . scanl (\(_, s) i -> (i, IntSet.insert i s)) (undefined, mempty)

isBingo :: IntSet -> [[Int]] -> Bool
isBingo s b = any (all (`IntSet.member` s)) $ b <> transpose b

playBingo :: [(Int, IntSet)] -> [[Int]] -> Maybe (Int, IntSet)
playBingo ms b = find ((`isBingo` b) . snd) ms

-- >>> part1 parsedExample
-- 4512
-- >>> part1 parsedProblem
-- 2745
part1 :: ([Int], [[[Int]]]) -> Int
part1 (ms, bs) = score $ minimumOn (IntSet.size . snd . snd) $ mapMaybe (\b -> (b,) <$> playBingo bms b) bs
  where
    bms = buildMoves ms
    score (b, (w, ws)) = w * sum (filter (not . (`IntSet.member` ws)) $ concat b)
