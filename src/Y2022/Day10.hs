module Y2022.Day10 where

import AocUtil
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many)

puzzle :: Puzzle [Instruction]
puzzle =
    Puzzle
        { year = "2022"
        , day = "10"
        , parser = program
        , parts = [Part part1 13140 12980]
        }

test_ :: TestTree
test_ = tests puzzle

data Instruction
    = Noop
    | Addx Int
    deriving (Show)

-- >>> parseThrowIO program "_" "noop\naddx -1\n"
-- [Noop,Addx (-1)]
program :: Parsec Void Text [Instruction]
program = endBy instruction eol <* eof

instruction :: Parsec Void Text Instruction
instruction = ("noop" $> Noop) <|> ("addx " *> (Addx <$> signedInt))

execInstruction :: Instruction -> [Sum Int]
execInstruction Noop = [0]
execInstruction (Addx n) = [0, Sum n]

-- >>> ticks [Noop, Addx (3), Addx (-5)]
-- [(0,Sum {getSum = 1}),(1,Sum {getSum = 1}),(2,Sum {getSum = 1}),(3,Sum {getSum = 4}),(4,Sum {getSum = 4}),(5,Sum {getSum = -1})]
ticks :: [Instruction] -> [(Int, Sum Int)]
ticks = scanl (\(ip, x) dx -> (succ ip, x <> dx)) (1, 1) . concatMap execInstruction

part1 :: [Instruction] -> Int
part1 is = getSum $ foldMap (\(ip, x) -> Sum ip * x) $ filter (interesting . fst) ts
  where
    interesting i = i == 20 || (i > 20 && mod (i - 20) 40 == 0)
    ts = ticks is
