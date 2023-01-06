module Y2022.Day14 where

import AocUtil
import Data.Set qualified as Set
import MegaParsecUtil
import Relude.Extra (maximum1)
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

type Coord = (Int, Int)

puzzle :: Puzzle [[Coord]]
puzzle =
    Puzzle
        { year = "2022"
        , day = "14"
        , parser = endBy path eol <* eof
        , parts = [Part (simulate . drawPaths) 24 793]
        }

test_ :: TestTree
test_ = tests puzzle

path :: Parsec Void Text [Coord]
path = sepBy point " -> "

point :: Parsec Void Text Coord
point = (,) <$> int <*> ("," *> int)

type Grid = Set Coord

drawLine :: Coord -> Coord -> Grid
drawLine (x0, y0) (x1, y1) =
    Set.fromList $
        if x0 == x1
            then map (x0,) [min y0 y1 .. max y0 y1]
            else map (,y0) [min x0 x1 .. max x0 x1]

drawPath :: [Coord] -> Grid
drawPath xs = mconcat $ zipWith drawLine xs (drop 1 xs)

drawPaths :: [[Coord]] -> Grid
drawPaths = foldMap drawPath

directions :: Coord -> [Coord]
directions (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

sandStep :: Grid -> Coord -> Coord
sandStep g c = fromMaybe c $ find (`Set.notMember` g) $ directions c

sandFinal :: Int -> Grid -> Coord -> Maybe Coord
sandFinal ymax g = go
  where
    go c@(_, y)
        | y > ymax = Nothing
        | c == c' = Just c
        | otherwise = go c'
      where
        c' = sandStep g c

simulate :: Grid -> Int
simulate g0 = go 0 g0
  where
    ymax = fromMaybe 0 $ viaNonEmpty (maximum1 . fmap snd) $ Set.toList g0
    go !n g = case sandFinal ymax g (500, 0) of
        Nothing -> n
        Just s -> go (succ n) (Set.insert s g)
