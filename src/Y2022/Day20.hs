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
        , parts =
            [ Part (solve 1 1) (Right 3) (Right 3346)
            , Part (solve 10 811589153) (Right 1623178306) (Right 4265712588168)
            ]
        }

test_ :: TestTree
test_ = tests puzzle

test_part1_steps :: TestTree
test_part1_steps =
    withResource (puzzleInput puzzle Example) (const $ pure ()) $ \gi ->
        let iters :: Int -> Mixer -> Either Text Mixer
            iters 0 m = pure m
            iters i m = iters (i - 1) =<< iteration m
            tc i xs =
                testCase ("Iter " <> show i) $ do
                    m <- gi
                    fmap (map element . toList) (iters i m) @?= Right xs
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
                , tc 1 [2, 1, negate 3, 3, negate 2, 0, 4]
                , tc 2 [1, negate 3, 2, 3, negate 2, 0, 4]
                , tc 3 [1, 2, 3, negate 2, negate 3, 0, 4]
                , tc 4 [1, 2, negate 2, negate 3, 0, 3, 4]
                , tc 5 [1, 2, negate 3, 0, 3, 4, negate 2]
                , tc 6 [1, 2, negate 3, 0, 3, 4, negate 2]
                , tc 7 [1, 2, negate 3, 4, 0, 3, negate 2]
                ]

type Mixer = FT.FingerTree M E

data Least = Least {generation :: Int, position :: Int} | Missing
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

solve :: Int -> Int -> Mixer -> Either Text RelPos
solve generations decryptionKey m0 = score =<< go (FT.fmap' decrypt m0)
  where
    decrypt x@E{element} = x{element = element * coerce decryptionKey}
    score m =
        let pos n = element <$> index (AbsPos $ n `mod` coerce (fst $ measure m)) m
         in do
                zp <- case elemIndex 0 $ map element $ toList m of
                    Just i -> pure i
                    Nothing -> Left "no zero element"
                fold <$> sequence [pos $ zp + 1000, pos $ zp + 2000, pos $ zp + 3000]
    go m = do
        m' <- iteration m
        case snd $ measure m' of
            Least{generation} | generation >= generations -> pure m'
            _ -> go m'

iteration :: Mixer -> Either Text Mixer
iteration m =
    maybe
        (pure m)
        ( \i -> do
            rp <- element <$> index i m
            let j = relPos rp i $ pred $ fst $ measure m
            move i j m
        )
        $ getNext m
mkMixer :: [Int] -> Mixer
mkMixer = FT.fromList . zipWith (\i n -> E (RelPos n) (Least 0 i)) [0 ..]

getNext :: Mixer -> Maybe AbsPos
getNext xs = case measure xs of
    (_, x@(Least{})) ->
        let below y = y <= x
         in Just $ coerce $ fst $ measure $ fst $ FT.split (below . snd) xs
    _ -> Nothing

slice :: AbsPos -> Mixer -> (Mixer, Mixer)
slice (AbsPos n) = FT.split (\(m, _) -> m > Size n)

relPos :: RelPos -> AbsPos -> Size -> AbsPos
relPos (RelPos n) (AbsPos i) (Size sz) = AbsPos $ case (n + i) `mod` sz of
    0 -> sz
    x -> x

move :: AbsPos -> AbsPos -> Mixer -> Either Text Mixer
move i j xs = cutOut i xs <&> \(x, xs') -> putIn j (x{moveOrder = nextGen $ moveOrder x}) xs'

nextGen :: Least -> Least
nextGen (Least{generation, position}) = Least{generation = succ generation, position}
nextGen Missing = Missing

cutOut :: AbsPos -> Mixer -> Either Text (E, Mixer)
cutOut i xs = case slice i xs of
    (l, FT.viewl -> x FT.:< r) -> pure (x, l <> r)
    _ -> Left "Index out of bounds"

index :: AbsPos -> Mixer -> Either Text E
index i xs = fst <$> cutOut i xs

putIn :: AbsPos -> E -> Mixer -> Mixer
putIn i x xs = case slice i xs of
    (l, r) -> l <> FT.singleton x <> r
