{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.FileEmbed (embedFile)
import Data.List (tails)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

example :: Text
example = T.decodeUtf8 $(embedFile "2021/day1/day1.example.txt")

problem :: Text
problem = T.decodeUtf8 $(embedFile "2021/day1/day1.problem.txt")

-- >>> part1 example
-- 7
-- >>> part1 problem
-- 1548
part1 :: T.Text -> Int
part1 = increases . map (read @Int . T.unpack) . T.lines

-- >>> part2 example
-- 5
-- >>> part2 problem
-- 1589
part2 :: Text -> Int
part2 = increases . map sum . windows 3 . map (read @Int . T.unpack) . T.lines

increases :: Ord a => [a] -> Int
increases xs = sum $ zipWith (\x y -> fromEnum $ y > x) xs (drop 1 xs)

-- >>> windows 3 "ABCDEFGH"
-- ["ABC","BCD","CDE","DEF","EFG","FGH"]
windows :: Int -> [b] -> [[b]]
windows n = getZipList . traverse ZipList . take n . tails
