module Y2021.Day2 where

import Control.Monad
import Data.Char (isDigit)
import Data.FileEmbed (embedFile)
import Data.Functor
import Data.Monoid
import Data.Text qualified as T
import Data.Void
import Test.Tasty.HUnit
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Prelude hiding (Down)

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day2/day2.example.txt")

-- >>> parsedExample
-- [(Forward,5),(Down,5),(Forward,8),(Up,3),(Down,8),(Forward,2)]
parsedExample :: [(Direction, Int)]
parsedExample = either (error . toText . errorBundlePretty) id $ parse movements "example" example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day2/day2.problem.txt")

parsedProblem :: [(Direction, Int)]
parsedProblem = either (error . toText . errorBundlePretty) id $ parse movements "problem" problem

-- >>> either (Left . errorBundlePretty) Right $ parse (int <* eof) "" "1234"
-- Right 1234
int :: Parsec Void Text Int
int = maybe (fail "invalid number") pure . (readMaybe . toString) =<< takeWhile1P (Just "digit") isDigit

data Direction = Forward | Down | Up
    deriving (Show)

direction :: Parsec Void Text Direction
direction = msum [string (T.toLower $ show d) $> d | d <- [Forward, Down, Up]]

-- >>> either (Left . errorBundlePretty) Right $ parse (movement <* eof) "" "down 1234"
-- Right (Down,1234)
movement :: Parsec Void Text (Direction, Int)
movement = liftA2 (,) (direction <* space) int

-- >>> either (Left . errorBundlePretty) Right $ parse (movements <* eof) "" example
-- Right [(Forward,5),(Down,5),(Forward,8),(Up,3),(Down,8),(Forward,2)]
movements :: Parsec Void Text [(Direction, Int)]
movements = some (movement <* (void eol <|> eof))

-- >>> map move parsedExample
-- [(Sum {getSum = 5},Sum {getSum = 0}),(Sum {getSum = 0},Sum {getSum = 5}),(Sum {getSum = 8},Sum {getSum = 0}),(Sum {getSum = 0},Sum {getSum = -3}),(Sum {getSum = 0},Sum {getSum = 8}),(Sum {getSum = 2},Sum {getSum = 0})]
move :: (Direction, Int) -> (Sum Int, Sum Int)
move (d, n) = case d of
    Up -> (0, Sum $ negate n)
    Down -> (0, Sum n)
    Forward -> (Sum n, 0)

-- >>> part1 parsedExample
-- 150
-- >>> part1 parsedProblem
-- 1692075
part1 :: [(Direction, Int)] -> Int
part1 = getSum . uncurry (*) . foldMap move

type Aim = Sum Int

aimMove :: Aim -> (Sum Int, Sum Int) -> (Aim, (Sum Int, Sum Int))
aimMove a (x, y) = (a <> y, (x, x * (a <> y)))

-- >>> part2 parsedExample
-- 900
-- >>> part2 parsedProblem
-- 1749524700
part2 :: [(Direction, Int)] -> Int
part2 = getSum . uncurry (*) . mconcat . snd . mapAccumL aimMove mempty . map move

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 150

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 1692075

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= 900

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= 1749524700
