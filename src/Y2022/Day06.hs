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
        , parts =
            [ Part (solve 4) (Just 7) (Just 1142)
            , Part (solve 14) (Just 19) (Just 2803)
            ]
        }

test_ :: TestTree
test_ = tests puzzle

windows :: Int -> [a] -> [[a]]
windows n0 xs0 = map ($ []) $ go n0
  where
    xs' = map (:) xs0
    go 1 = xs'
    go n = zipWith (.) (replicate (n - 1) id <> xs') (go $ n - 1)

solve :: Int -> Text -> Maybe Integer
solve n = fmap fst . find (ok . snd) . zip [1 ..] . windows n . toString
  where
    ok = (n ==) . Set.size . Set.fromList
