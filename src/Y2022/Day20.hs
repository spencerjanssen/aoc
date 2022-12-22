{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Y2022.Day20 where

import AocUtil
import Data.FingerTree (Measured (..))
import Data.FingerTree qualified as FT
import Data.List (elemIndex)
import Data.Semigroup
import MegaParsecUtil
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle Mixer
puzzle =
    Puzzle
        { year = "2022"
        , day = "20"
        , parser = mkMixer <$> endBy signedInt eol <* eof
        , parts = [Part part1 (Right 3) (Right 3346)]
        }

test_ :: TestTree
test_ = tests puzzle

test_part1_steps :: TestTree
test_part1_steps =
    withResource (puzzleInput puzzle Example) (const $ pure ()) $ \gi ->
        let iters :: Int -> Mixer -> Either Text Mixer
            iters 0 m = pure m
            iters i m = maybe (pure m) (iters (i - 1) =<<) $ iteration m
            tc i xs =
                testCase ("Iter " <> show i) $ do
                    m <- gi
                    iters i m @?= Right (FT.fromList xs)
            tcI i n = testCase ("Index " <> show i) $ do
                Right m <- iters i <$> gi
                getNext m @?= Just n
            tcV n i v = testCase ("Value " <> show n) $ do
                Right m <- iters n <$> gi
                element <$> index i m @?= Right v
            tcR n i v ap = testCase ("RelPos " <> show n) $ do
                Right m <- iters n <$> gi
                let found = relPos v i (pred $ fst $ measure m)
                found @?= ap
         in testGroup
                "Manual"
                [ tcI 0 0
                , tcV 0 0 1
                , tcR 0 0 1 1
                , tcI 4 2
                , tcV 4 2 (negate 2)
                , tcR 4 2 (negate 2) 6
                , tcI 6 5
                , tcV 6 5 4
                , tcR 6 5 4 3
                , tc
                    1
                    [ E 2 (Least 1)
                    , E 1 Missing
                    , E (negate 3) (Least 2)
                    , E 3 (Least 3)
                    , E (negate 2) (Least 4)
                    , E 0 (Least 5)
                    , E 4 (Least 6)
                    ]
                , tc
                    2
                    [ E 1 Missing
                    , E (negate 3) (Least 2)
                    , E 2 Missing
                    , E 3 (Least 3)
                    , E (negate 2) (Least 4)
                    , E 0 (Least 5)
                    , E 4 (Least 6)
                    ]
                , tc
                    3
                    [ E 1 Missing
                    , E 2 Missing
                    , E 3 (Least 3)
                    , E (negate 2) (Least 4)
                    , E (negate 3) Missing
                    , E 0 (Least 5)
                    , E 4 (Least 6)
                    ]
                , tc
                    4
                    [ E 1 Missing
                    , E 2 Missing
                    , E (negate 2) (Least 4)
                    , E (negate 3) Missing
                    , E 0 (Least 5)
                    , E 3 Missing
                    , E 4 (Least 6)
                    ]
                , tc
                    5
                    [ E 1 Missing
                    , E 2 Missing
                    , E (negate 3) Missing
                    , E 0 (Least 5)
                    , E 3 Missing
                    , E 4 (Least 6)
                    , E (negate 2) Missing
                    ]
                , tc
                    6
                    [ E 1 Missing
                    , E 2 Missing
                    , E (negate 3) Missing
                    , E 0 Missing
                    , E 3 Missing
                    , E 4 (Least 6)
                    , E (negate 2) Missing
                    ]
                , tc
                    7
                    [ E 1 Missing
                    , E 2 Missing
                    , E (negate 3) Missing
                    , E 4 Missing
                    , E 0 Missing
                    , E 3 Missing
                    , E (negate 2) Missing
                    ]
                ]

type Mixer = FT.FingerTree M E

data Least = Least Int | Missing
    deriving (Eq, Ord, Show)
    deriving (Semigroup) via Min Least

instance Monoid Least where
    mempty = Missing

newtype AbsPos = AbsPos Int
    deriving (Eq, Ord, Read, Show)
    deriving newtype (Num, Enum)

newtype RelPos = RelPos Int
    deriving (Eq, Ord, Read, Show)
    deriving newtype (Num)
    deriving (Semigroup, Monoid) via (Sum Int)

newtype Size = Size Int
    deriving (Eq, Ord, Read, Show)
    deriving newtype (Enum)
    deriving (Semigroup, Monoid) via (Sum Int)

data E = E
    { element :: RelPos
    , moveOrder :: Least
    }
    deriving (Show, Eq)

type M = (Size, Least)

instance Measured M E where
    measure E{moveOrder} = (Size 1, moveOrder)

part1 :: Mixer -> Either Text RelPos
part1 m0 = score =<< go m0
  where
    score m =
        let pos n = element <$> index (AbsPos $ n `mod` coerce (fst $ measure m)) m
         in do
                zp <- case elemIndex 0 $ map element $ toList m of
                    Just i -> pure i
                    Nothing -> Left "no zero element"
                fold <$> sequence [pos $ zp + 1000, pos $ zp + 2000, pos $ zp + 3000]
    go m = maybe (pure m) (go =<<) $ iteration m

iteration :: Mixer -> Maybe (Either Text Mixer)
iteration m =
    getNext m <&> \i -> do
        rp <- element <$> index i m
        let j = relPos rp i $ pred $ fst $ measure m
        move i j m

mkMixer :: [Int] -> Mixer
mkMixer = FT.fromList . zipWith (\i n -> E (RelPos n) (Least i)) [0 ..]

getNext :: Mixer -> Maybe AbsPos
getNext xs = case measure xs of
    (_, Least x) ->
        let below Missing = False
            below (Least y) = y <= x
         in Just $ coerce $ fst $ measure $ fst $ FT.split (below . snd) xs
    _ -> Nothing

slice :: AbsPos -> Mixer -> (Mixer, Mixer)
slice (AbsPos n) = FT.split (\(m, _) -> m > Size n)

relPos :: RelPos -> AbsPos -> Size -> AbsPos
relPos (RelPos n) (AbsPos i) (Size sz) = AbsPos $ case (n + i) `mod` sz of
    0 -> sz
    x -> x

move :: AbsPos -> AbsPos -> Mixer -> Either Text Mixer
move i j xs = cutOut i xs <&> \(x, xs') -> putIn j (x{moveOrder = Missing}) xs'

cutOut :: AbsPos -> Mixer -> Either Text (E, Mixer)
cutOut i xs = case slice i xs of
    (l, FT.viewl -> x FT.:< r) -> pure (x, l <> r)
    _ -> Left "Index out of bounds"

index :: AbsPos -> Mixer -> Either Text E
index i xs = fst <$> cutOut i xs

putIn :: AbsPos -> E -> Mixer -> Mixer
putIn i x xs = case slice i xs of
    (l, r) -> l <> FT.singleton x <> r
