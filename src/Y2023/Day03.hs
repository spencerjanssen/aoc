module Y2023.Day03 where

import AocUtil
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import MegaParsecUtil
import Relude.Extra.Map
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (First (..), Last (..), many, round, some)

puzzle :: Puzzle Schematic
puzzle =
    Puzzle
        { year = "2023"
        , day = "03"
        , parser = schematic
        , parts =
            [ Part part1 4361 543867
            , Part part2 467835 79613331
            ]
        }

test_ :: TestTree
test_ = tests puzzle

data PartPos = PartPos {lineNo, startCol, endCol :: Int}
    deriving (Show, Eq, Ord)

data Schematic = Schematic
    { partNumbers :: [(PartPos, Int)]
    , symbols :: Map (Int, Int) Char
    }
    deriving (Show)

schematic :: Parsec Void Text Schematic
schematic = frob . concat <$> (endBy line eol <* eof)
  where
    frob :: [Either ((Int, Int), Char) (PartPos, Int)] -> Schematic
    frob (partitionEithers -> (syms, partNumbers)) =
        Schematic
            { partNumbers
            , symbols = fromList syms
            }

line :: Parsec Void Text [Either ((Int, Int), Char) (PartPos, Int)]
line = concat <$> many (pure . Right <$> partNumber <|> blank <|> pure . Left <$> symbol)
  where
    sp2rc sp = (unPos sp.sourceLine, unPos sp.sourceColumn)
    blank = char '.' $> []
    partNumber = do
        (lineNo, startCol) <- sp2rc <$> getSourcePos
        i <- int
        endCol <- pred . snd . sp2rc <$> getSourcePos
        pure (PartPos{lineNo, startCol, endCol}, i)
    symbol = do
        start <- sp2rc <$> getSourcePos
        (start,) <$> nonNumericChar

part1 :: Schematic -> Int
part1 = sum . eligible

eligible :: Schematic -> [Int]
eligible s = [pn | (pp, pn) <- s.partNumbers, nearSymbol pp]
  where
    nearSymbol pp = any (`member` s.symbols) $ near pp

near :: PartPos -> [(Int, Int)]
near pp = [(ln, col) | ln <- [pred pp.lineNo .. succ pp.lineNo], col <- [pred pp.startCol .. succ pp.endCol]]

part2 :: Schematic -> Int
part2 = sum . map (product . map snd . Set.toList) . allGears

allGears :: Schematic -> [Set (PartPos, Int)]
allGears = filter isGear . toList . nearGears

isGear :: (Ord a) => Set a -> Bool
isGear = (2 ==) . size

nearGears :: Schematic -> Map (Int, Int) (Set (PartPos, Int))
nearGears s =
    Map.fromListWith
        (<>)
        [ (rc, one (pp, pn))
        | (pp, pn) <- s.partNumbers
        , rc <- near pp
        , '*' <- maybeToList $ lookup rc s.symbols
        ]
