module Y2022.Day13 where

import AocUtil
import MegaParsecUtil
import Test.Tasty (TestTree)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle [(Packet, Packet)]
puzzle =
    Puzzle
        { year = "2022"
        , day = "13"
        , parser = pairs
        , parts = [Part part1 13 6369, Part part2 140 25800]
        }

test_ :: TestTree
test_ = tests puzzle

data Packet
    = Int Int
    | List [Packet]
    deriving (Show)

instance Eq Packet where
    x == y = compare x y == EQ

instance Ord Packet where
    compare (Int x) (Int y) = compare x y
    compare (List xs) (List ys) = compare xs ys
    compare x@(Int _) ys = compare (List $ pure x) ys
    compare xs y@(Int _) = compare xs (List $ pure y)

pairs :: Parsec Void Text [(Packet, Packet)]
pairs = sepBy pair eol <* eof

pair :: Parsec Void Text (Packet, Packet)
pair = (,) <$> packet <* eol <*> packet <* eol

packet :: Parsec Void Text Packet
packet = list <|> Int <$> int
  where
    list = List <$> (char '[' *> sepBy packet (char ',') <* char ']')

part1 :: [(Packet, Packet)] -> Int
part1 ps = sum [i | (i, (x, y)) <- zip [1 ..] ps, x <= y]

part2 :: [(Packet, Packet)] -> Int
part2 ps =
    product
        [ i
        | (i, p) <- zip [1 ..] $ sort $ sigPackets <> concat [[x, y] | (x, y) <- ps]
        , p `elem` sigPackets
        ]
  where
    sig n = List [List [Int n]]
    sigPackets = [sig 2, sig 6]
