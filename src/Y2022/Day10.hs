module Y2022.Day10 where

import AocUtil
import Data.Set qualified as Set
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
        , parts = [Part part1 13140 12980, Part render part2ExampleOutput part2ProblemOutput]
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

-- >>> map cyclePosition [1, 20, 40, 41, 240]
-- [(0,0),(0,19),(0,39),(1,0),(5,39)]
cyclePosition :: Int -> (Int, Int)
cyclePosition n = divMod (n - 1) 40

spritePosition :: Int -> [Int]
spritePosition n = map (n +) [-1, 0, 1]

lit ::
    -- | Cycle
    Int ->
    -- | X counter
    Int ->
    Maybe (Int, Int)
lit i x = do
    let (r, c) = cyclePosition i
        ps = spritePosition x
    guard $ c `elem` ps
    pure (r, c)

draw :: (Int, Int) -> Set (Int, Int) -> Text
draw (rs, cs) s =
    unlines
        [ fromString [if Set.member (r, c) s then '#' else '.' | c <- [0 .. cs - 1]]
        | r <- [0 .. rs - 1]
        ]

render :: [Instruction] -> Text
render is = draw (6, 40) ls
  where
    ts = ticks is
    ls = fromList $ mapMaybe (\(i, Sum x) -> lit i x) ts

part2ExampleOutput :: Text
part2ExampleOutput =
    "##..##..##..##..##..##..##..##..##..##..\n\
    \###...###...###...###...###...###...###.\n\
    \####....####....####....####....####....\n\
    \#####.....#####.....#####.....#####.....\n\
    \######......######......######......####\n\
    \#######.......#######.......#######.....\n"

part2ProblemOutput :: Text
part2ProblemOutput =
    "###..###....##.#....####.#..#.#....###..\n\
    \#..#.#..#....#.#....#....#..#.#....#..#.\n\
    \###..#..#....#.#....###..#..#.#....#..#.\n\
    \#..#.###.....#.#....#....#..#.#....###..\n\
    \#..#.#.#..#..#.#....#....#..#.#....#....\n\
    \###..#..#..##..####.#.....##..####.#....\n"
