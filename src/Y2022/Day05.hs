module Y2022.Day05 where

import AocUtil
import Data.IntMap.Strict qualified as IntMap
import MegaParsecUtil
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle (IntMap Tower, [Move])
puzzle =
    Puzzle
        { year = "2022"
        , day = "05"
        , parser = cratesAndMoves
        , parts =
            [ Part (solve reverse) "CMZ" "WSFTMRHPP"
            , Part (solve id) "MCD" "GSLCMFBRP"
            ]
        }

test_ :: TestTree
test_ = tests puzzle

unit_tower_parse :: Assertion
unit_tower_parse = do
    (ts, _) <- puzzleInput puzzle Example
    ts @?= IntMap.fromList [(1, "NZ"), (2, "DCM"), (3, "P")]

type Tower = [Char]

cratesAndMoves :: Parsec Void Text (IntMap Tower, [Move])
cratesAndMoves = do
    cs <- endBy crates eol
    ls <- labels
    void eol
    void eol
    ms <- endBy move eol
    eof
    pure (IntMap.fromList $ zip [1 ..] $ toTowers (length ls) cs, ms)

toTowers :: Int -> [[Maybe Char]] -> [Tower]
toTowers n = map catMaybes . transpose . map (padTo n)

padTo :: Int -> [Maybe a] -> [Maybe a]
padTo n xs = zipWith (<|>) (xs <> repeat Nothing) $ replicate n Nothing

crates :: Parsec Void Text [Maybe Char]
crates = sepBy (Just <$> crate <|> Nothing <$ "   ") " "

crate :: Parsec Void Text Char
crate = char '[' *> asciiAlphaChar <* char ']'

labels :: Parsec Void Text [Int]
labels = sepBy (" " *> int <* " ") " "

data Move = Move {moves, from, to :: Int}

move :: Parsec Void Text Move
move = do
    void "move "
    moves <- int
    void " from "
    from <- int
    void " to "
    to <- int
    pure Move{..}

runMove :: (Tower -> Tower) -> IntMap Tower -> Move -> IntMap Tower
runMove f t Move{moves, from, to} = IntMap.insertWith (<>) to (f taken) $ IntMap.insert from remain t
  where
    (taken, remain) = splitAt moves $ fromMaybe [] $ IntMap.lookup from t

solve :: (Tower -> Tower) -> (IntMap Tower, [Move]) -> String
solve f (ts, ms) = mapMaybe listToMaybe $ toList $ foldl' (runMove f) ts ms
