module Y2022.Day07 where

import AocUtil
import Data.Map.Strict qualified as Map
import Data.Semigroup
import Data.Sequence qualified as Seq
import MegaParsecUtil
import Test.Tasty (TestTree)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle [Either Command [DirLine]]
puzzle =
    Puzzle
        { year = "2022"
        , day = "07"
        , parser = session
        , parts =
            [ Part part1 95437 1611443
            , Part part2 24933642 2086088
            ]
        }

test_ :: TestTree
test_ = tests puzzle

data Command
    = Ls
    | Cd CdTarget

data CdTarget
    = Root
    | Parent
    | Rel Text

type Session = [Either Command DirLine]

session :: Parsec Void Text [Either Command [DirLine]]
session = manyTill (Left <$> command <|> Right <$> dirResponse) eof

command :: Parsec Void Text Command
command = "$ " *> ("ls" $> Ls <|> Cd <$> ("cd " *> cdTarget)) <* eol

cdTarget :: Parsec Void Text CdTarget
cdTarget = Root <$ "/" <|> Parent <$ ".." <|> Rel <$> alphaNumDot

data DirLine = Dir Text | File Int Text

dirResponse :: Parsec Void Text [DirLine]
dirResponse = manyTill (dirLine <* eol) (notFollowedBy alphaNumDot)

dirLine :: Parsec Void Text DirLine
dirLine = (Dir <$> ("dir " *> alphaNumDot)) <|> (File <$> int <*> (space *> alphaNumDot))

nav :: CdTarget -> Seq Text -> Seq Text
nav = \case
    Root -> const mempty
    Parent -> \s -> case Seq.viewr s of
        Seq.EmptyR -> mempty
        s' Seq.:> _ -> s'
    Rel x -> (Seq.|> x)

fileSizes :: [Either Command [DirLine]] -> [((Seq Text, Text), Int)]
fileSizes t = foldr @[] f (const []) t mempty
  where
    f (Left (Cd x)) next cwd = next $ nav x cwd
    f (Left _) next cwd = next cwd
    f (Right ds) next cwd = [((cwd, fn), sz) | File sz fn <- ds] <> next cwd

dirSizes :: [((Seq Text, Text), Int)] -> [(Seq Text, Int)]
dirSizes fszs = Map.toList $ Map.fromListWith (+) [(d', sz) | ((d, _f), sz) <- fszs, d' <- toList $ Seq.inits d]

part1 :: [Either Command [DirLine]] -> Int
part1 = sum . map snd . filter ((<= 100000) . snd) . dirSizes . fileSizes

part2 :: [Either Command [DirLine]] -> Min Int
part2 transcript = foldMap Min $ filter (>= tgt) $ map snd dszs
  where
    fszs = fileSizes transcript
    used = sum $ map snd fszs
    tgt = 30000000 - (70000000 - used)
    dszs = dirSizes fszs
