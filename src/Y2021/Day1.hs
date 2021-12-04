module Y2021.Day1 where

import Data.FileEmbed (embedFile)

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day1/day1.example.txt")

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day1/day1.problem.txt")

-- >>> part1 example
-- 7
-- >>> part1 problem
-- 1548
part1 :: Text -> Int
part1 = increases . map (readMaybe @Int . toString) . lines

-- >>> part2 example
-- 5
-- >>> part2 problem
-- 1589
part2 :: Text -> Int
part2 = increases . map sum . windows 3 . mapMaybe (readMaybe @Int . toString) . lines

increases :: Ord a => [a] -> Int
increases xs = sum $ zipWith (\x y -> fromEnum $ y > x) xs (drop 1 xs)

-- >>> windows 3 "ABCDEFGH"
-- ["ABC","BCD","CDE","DEF","EFG","FGH"]
windows :: Int -> [b] -> [[b]]
windows n = getZipList . traverse ZipList . take n . tails
