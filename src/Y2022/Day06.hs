module Y2022.Day06 where

import AocUtil
import Data.Set qualified as Set
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle Text
puzzle =
    Puzzle
        { year = "2022"
        , day = "06"
        , parser = alphaNum <* eol <* eof
        , parts = [Part part1 (Just 7) (Just 1142)]
        }

test_ :: TestTree
test_ = tests puzzle

windows :: Int -> [a] -> [[a]]
windows n0 xs0 = map ($ []) $ go n0
  where
    xs' = map (:) xs0
    go 1 = xs'
    go n = zipWith (.) (replicate (n - 1) id <> xs') (go $ n - 1)

part1 :: Text -> Maybe Integer
part1 = fmap fst . find (ok . snd) . zip [1 ..] . windows 4 . toString
  where
    ok = (4 ==) . Set.size . Set.fromList
