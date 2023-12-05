module Y2023.Day02 where

import AocUtil
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (First (..), Last (..), many, round, some)

puzzle :: Puzzle [(ID, [Cubes])]
puzzle =
    Puzzle
        { year = "2023"
        , day = "02"
        , parser = games
        , parts =
            [ Part solve 8 2593
            ]
        }

test_ :: TestTree
test_ = tests puzzle

type ID = Int

type Game = [Cubes]

data Cubes = Cubes
    { red :: Int
    , green :: Int
    , blue :: Int
    }

instance Semigroup Cubes where
    Cubes r1 g1 b1 <> Cubes r2 g2 b2 = Cubes (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid Cubes where
    mempty = Cubes 0 0 0

isSubsetOf :: Cubes -> Cubes -> Bool
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

cubes :: Parsec Void Text Cubes
cubes = fold <$> sepBy cube ", "

cube :: Parsec Void Text Cubes
cube = do
    i <- int
    void " "
    c <- color
    pure $ c i

color :: Parsec Void Text (Int -> Cubes)
color =
    ("red" $> \c -> mempty{red = c})
        <|> ("green" $> \c -> mempty{green = c})
        <|> ("blue" $> \c -> mempty{blue = c})

solve :: [(ID, Game)] -> Int
solve = sum . map fst . filter (all (`isSubsetOf` bag) . snd)
  where
    bag = Cubes{red = 12, green = 13, blue = 14}
