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
    Cubes r1 g1 b1 <> Cubes r2 g2 b2 = Cubes (r1 <> r2) (g1 <> g2) (b1 <> b2)

instance (Monoid (f Int)) => Monoid (Cubes f) where
    mempty = Cubes mempty mempty mempty

isSubsetOf :: (Ord (f Int)) => Cubes f -> Cubes f -> Bool
isSubsetOf (Cubes r1 g1 b1) (Cubes r2 g2 b2) = r1 <= r2 && g1 <= g2 && b1 <= b2

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
    power Cubes{red, green, blue} = getMax red * getMax green * getMax blue

toMax :: Cubes Sum -> Cubes Max
toMax Cubes{red, green, blue} =
    Cubes
        { red = Max $ getSum red
        , green = Max $ getSum green
        , blue = Max $ getSum blue
        }
