module Y2022.Day02 where

import Data.FileEmbed (embedFile)
import MegaParsecUtil
import Test.Tasty.HUnit
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char

example :: Text
example = decodeUtf8 $(embedFile "inputs/2022/day02/example.txt")

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2022/day02/problem.txt")

-- >>> parsedExample
-- [(A,Y),(B,X),(C,Z)]
parsedExample :: [(ABC, XYZ)]
parsedExample = parseThrow tournament "example" example

parsedProblem :: [(ABC, XYZ)]
parsedProblem = parseThrow tournament "problem" problem

data RPS = Rock | Paper | Scissors
    deriving (Show, Eq)

data ABC = A | B | C
    deriving (Show)

data XYZ = X | Y | Z
    deriving (Show)

tournament :: ParsecT Void Text Identity [(ABC, XYZ)]
tournament = endBy game eol <* eof

game :: Parsec Void Text (ABC, XYZ)
game = liftA2 (,) (abc <* char ' ') xyz

abc :: Parsec Void Text ABC
abc =
    choice
        [ char 'A' $> A
        , char 'B' $> B
        , char 'C' $> C
        ]

xyz :: Parsec Void Text XYZ
xyz =
    choice
        [ char 'X' $> X
        , char 'Y' $> Y
        , char 'Z' $> Z
        ]

decodeABC :: ABC -> RPS
decodeABC = \case
    A -> Rock
    B -> Paper
    C -> Scissors

decodeXYZ :: XYZ -> RPS
decodeXYZ = \case
    X -> Rock
    Y -> Paper
    Z -> Scissors

data Winner = Opponent | You | Draw
    deriving (Show)

-- >>> liftA2 (\o y -> (o, y, result o y)) [Rock, Paper, Scissors] [Rock, Paper, Scissors]
-- [(Rock,Rock,Draw),(Rock,Paper,Opponent),(Rock,Scissors,You),(Paper,Rock,Opponent),(Paper,Paper,Draw),(Paper,Scissors,You),(Scissors,Rock,You),(Scissors,Paper,Opponent),(Scissors,Scissors,Draw)]
result :: RPS -> RPS -> Winner
result opp you
    | opp == you = Draw
    | otherwise = case (opp, you) of
        (Rock, Scissors) -> Opponent
        (Paper, Rock) -> Opponent
        (Scissors, Paper) -> Opponent
        _ -> You

score :: RPS -> RPS -> Int
score opp you = scoreYou + scoreResult
  where
    scoreYou = case you of
        Rock -> 1
        Paper -> 2
        Scissors -> 3
    scoreResult = case result opp you of
        Opponent -> 0
        Draw -> 3
        You -> 6

-- >>> part1 parsedExample
-- 15
part1 :: [(ABC, XYZ)] -> Int
part1 = sum . map (\(x, a) -> score (decodeABC x) (decodeXYZ a))

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 15

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 12855
