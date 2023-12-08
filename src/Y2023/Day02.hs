{-# LANGUAGE UndecidableInstances #-}

module Y2023.Day02 where

import AocUtil
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (First (..), Last (..), many, round, some)

puzzle :: Puzzle [(ID, Game)]
puzzle =
    Puzzle
        { year = "2023"
        , day = "02"
        , parser = games
        , parts =
            [ Part part1 8 2593
            , Part part2 2286 54699
            ]
        }

test_ :: TestTree
test_ = tests puzzle

type ID = Int

type Game = [Cubes Sum]

data Cubes f = Cubes
    { red :: f Int
    , green :: f Int
    , blue :: f Int
    }

instance (Semigroup (f Int)) => Semigroup (Cubes f) where
    x <> y =
        Cubes
            { red = x.red <> y.red
            , blue = x.blue <> y.blue
            , green = x.green <> y.green
            }

instance (Monoid (f Int)) => Monoid (Cubes f) where
    mempty = Cubes mempty mempty mempty

isSubsetOf :: (Ord (f Int)) => Cubes f -> Cubes f -> Bool
isSubsetOf x y = x.red <= y.red && x.green <= y.green && x.blue <= y.blue

games :: Parsec Void Text [(ID, Game)]
games = endBy game eol <* eof

game :: Parsec Void Text (ID, Game)
game = do
    void "Game "
    i <- int
    void ": "
    cs <- sepBy cubes "; "
    pure (i, cs)

cubes :: Parsec Void Text (Cubes Sum)
cubes = fold <$> sepBy cube ", "

cube :: Parsec Void Text (Cubes Sum)
cube = do
    i <- int
    void " "
    c <- color
    pure $ c $ Sum i

color :: Parsec Void Text (Sum Int -> Cubes Sum)
color =
    ("red" $> \c -> mempty{red = c})
        <|> ("green" $> \c -> mempty{green = c})
        <|> ("blue" $> \c -> mempty{blue = c})

part1 :: [(ID, Game)] -> Int
part1 = sum . map fst . filter (all (`isSubsetOf` bag) . snd)
  where
    bag = Cubes{red = 12, green = 13, blue = 14}

part2 :: [(ID, Game)] -> Int
part2 = sum . map (power . foldMap toMax . snd)
  where
    power c = c.red.getMax * c.green.getMax * c.blue.getMax

toMax :: Cubes Sum -> Cubes Max
toMax c =
    Cubes
        { red = Max c.red.getSum
        , green = Max c.green.getSum
        , blue = Max c.blue.getSum
        }
