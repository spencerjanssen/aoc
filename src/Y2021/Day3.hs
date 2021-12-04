module Y2021.Day3 where

import Control.Applicative
import Data.Coerce (coerce)
import Data.FileEmbed (embedFile)
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

example :: Text
example = T.decodeUtf8 $(embedFile "inputs/2021/day3/day3.example.txt")

parsedExample :: [[Int]]
parsedExample = parse example

problem :: Text
problem = T.decodeUtf8 $(embedFile "inputs/2021/day3/day3.problem.txt")

parsedProblem :: [[Int]]
parsedProblem = parse problem

-- >>> parse example
-- [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1],[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0],[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]]
parse :: Text -> [[Int]]
parse = map (map bit . T.unpack) . T.lines

bit :: Char -> Int
bit '0' = 0
bit '1' = 1
bit _ = error "invalid bit"

-- >>> summarize parsedExample
-- [7,5,8,7,5]
summarize :: [[Int]] -> [Int]
summarize = coerce . mconcat . coerce @_ @[Ap ZipList (Sum Int)]

-- >>> gamma parsedExample
-- [True,False,True,True,False]
gamma :: [[Int]] -> [Bool]
gamma xs = [l - i < i | i <- summarize xs]
  where
    l = length xs

-- >>> epsilon parsedExample
-- [False,True,False,False,True]
epsilon :: [[Int]] -> [Bool]
epsilon = map not . gamma

bitString :: [Bool] -> Int
bitString = sum . zipWith (\i b -> fromEnum b * 2 ^ i) [0 :: Int ..] . reverse

part1 :: [[Int]] -> Int
part1 x = product . map bitString $ [epsilon x, gamma x]

-- >>> result1
-- 852500
result1 :: Int
result1 = part1 parsedProblem

data T = T {elements :: Int, zero, one :: Maybe T}
    deriving (Show)

instance Semigroup T where
    T xe xz xo <> T ye yz yo = T (xe + ye) (xz <> yz) (xo <> yo)

instance Monoid T where
    mempty = T 0 Nothing Nothing

-- >>> fromBits [True, True]
-- T {elements = 1, zero = Z, one = T {elements = 1, zero = Z, one = Z}}
fromBits :: [Bool] -> T
fromBits = foldr f (T 1 Nothing Nothing)
  where
    f x xs = (if x then id else flip) (T 1) mempty (Just xs)

-- >>> select (<=) $ foldMap (fromBits . map toEnum) parsedExample
-- [True,False,True,True,True]
select :: (Int -> Int -> Bool) -> T -> [Bool]
select _ (T _ Nothing Nothing) = []
select p (T _ z o)
    | p (maybe 0 elements z) (maybe 0 elements o) = True : maybe [] (select p) o
    | otherwise = False : maybe [] (select p) z

-- >>> part2 parsedExample
-- 230
-- >>> part2 parsedProblem
-- 1007985
part2 :: [[Int]] -> Int
part2 p = product $ map bitString [select (<=) t, select co2 t]
  where
    t = foldMap (fromBits . map toEnum) p
    co2 0 _ = True
    co2 _ 0 = False
    co2 x y = case compare x y of
        EQ -> False
        GT -> True
        LT -> False
