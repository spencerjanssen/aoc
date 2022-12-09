module Y2022.Day09 where

import AocUtil
import Data.Set qualified as Set
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char

puzzle :: Puzzle [(Direction, Int)]
puzzle =
    Puzzle
        { year = "2022"
        , day = "09"
        , parts = [Part part1 13 6314]
        , parser = vectors
        }

test_ :: TestTree
test_ = tests puzzle

data Direction = U | D | L | R
    deriving (Show)

-- >>> puzzleInput puzzle Example
-- [(R,4),(U,4),(L,3),(D,1),(R,4),(D,1),(L,5),(R,2)]
vectors :: Parsec Void Text [(Direction, Int)]
vectors = endBy vector eol <* eof

vector :: Parsec Void Text (Direction, Int)
vector = (,) <$> direction <*> (space *> int)

direction :: Parsec Void Text Direction
direction =
    choice
        [ char 'U' $> U
        , char 'D' $> D
        , char 'L' $> L
        , char 'R' $> R
        ]

data Pos = Pos Int Int
    deriving (Show, Eq, Ord)

data Vec = Vec Int Int
    deriving (Show)

vec :: Pos -> Pos -> Vec
vec (Pos tx ty) (Pos hx hy) = Vec (hx - tx) (hy - ty)

move :: Vec -> Pos -> Pos
move (Vec dx dy) (Pos x y) = Pos (dx + x) (dy + y)

advanceTail :: Pos -> Pos -> Pos
advanceTail t h
    | abs dx > 1 || abs dy > 1 = move (Vec (signum dx) (signum dy)) t
    | otherwise = t
  where
    Vec dx dy = vec t h

directionVec :: Direction -> Vec
directionVec = \case
    U -> Vec 1 0
    D -> Vec (-1) 0
    L -> Vec 0 (-1)
    R -> Vec 0 1

part1Positions :: [(Direction, Int)] -> [(Pos, Pos)]
part1Positions ds = scanl' go (Pos 0 0, Pos 0 0) ds'
  where
    ds' = map directionVec $ concatMap (uncurry $ flip replicate) ds
    go (h, t) d =
        let h' = move d h
         in (h', advanceTail t h')

-- >>> part1 <$> puzzleInput puzzle Example
-- 13
part1 :: [(Direction, Int)] -> Int
part1 = Set.size . fromList . map snd . part1Positions
