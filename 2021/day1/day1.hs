{-# LANGUAGE TypeApplications #-}

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.List (tails)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

part1 f =
    print . increases . map (read @Int . T.unpack) . T.lines =<< TIO.readFile f

increases xs = sum $ zipWith (\x y -> fromEnum $ y > x) xs (drop 1 xs)

part2 f =
    print . increases . map sum . windows 3 . map (read @Int . T.unpack) . T.lines =<< TIO.readFile f

-- >>> windows 3 "ABCDEFGH"
-- ["ABC","BCD","CDE","DEF","EFG","FGH"]
windows n = getZipList . traverse ZipList . take n . tails
