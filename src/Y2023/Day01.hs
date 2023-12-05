{-# LANGUAGE BlockArguments #-}

module Y2023.Day01 where

import AocUtil
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty (TestTree)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (First (..), Last (..), many, round, some)

puzzle :: Puzzle [[Either Char Int]]
puzzle =
    Puzzle
        { year = "2023"
        , day = "01"
        , parser = block
        , parts =
            [ Part solve (Just 142) (Just 54940)
            ]
        }

test_ :: TestTree
test_ = tests puzzle

solve :: [[Either Char Int]] -> Maybe Int
solve = fmap getSum . foldMap sline
  where
    sline =
        foldMap (either mempty \d -> Just (First d, Last d))
            >>> fmap \(First mf, Last ml) -> Sum (mf * 10 + ml)

block :: Parsec Void Text [[Either Char Int]]
block = endBy (many tok) eol <* eof

tok :: Parsec Void Text (Either Char Int)
tok = (Left <$> asciiAlphaChar) <|> (Right <$> digit)
