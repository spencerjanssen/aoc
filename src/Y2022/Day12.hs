module Y2022.Day12 where

import Algebra.Graph.Label qualified as G
import Algebra.Graph.Labelled.AdjacencyMap as G
import AocUtil
import Data.Char (isAsciiLower)
import Data.Map.Strict qualified as Map
import Data.OrdPSQ qualified as PQ
import Data.Set qualified as Set
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle ((Int, Int), (Int, Int), AdjacencyMap (G.Distance Int) (Int, Int))
puzzle =
    Puzzle
        { year = "2022"
        , day = "12"
        , parser = topomap
        , parts = [Part part1 31 394]
        }

test_ :: TestTree
test_ = tests puzzle

newtype Elevation = Elevation Char
    deriving (Eq, Ord, Enum, Show)

data POI = S | E
    deriving (Eq, Show)

topomap :: Parsec Void Text ((Int, Int), (Int, Int), AdjacencyMap (G.Distance Int) (Int, Int))
topomap = do
    ess <- some (some (Right <$> elevation <|> Left <$> poi) <* eol) <* eof
    let lbl = concat $ zipWith (\i es -> zipWith (\j e -> ((i, j), e)) [0 ..] es) [0 ..] ess
    start <- case find ((Left S ==) . snd) lbl of
        Nothing -> fail "expected a starting point"
        Just ((i, j), _) -> pure (i, j)
    end <- case find ((Left E ==) . snd) lbl of
        Nothing -> fail "expected an ending point"
        Just ((i, j), _) -> pure (i, j)
    let poie S = Elevation 'a'
        poie E = Elevation 'z'
        elbl = Map.fromList $ map (second $ either poie id) lbl
        g =
            G.edges
                [ (1 :: G.Distance Int, u, v)
                | (u, e) <- Map.assocs elbl
                , v <- surrounding u
                , e' <- maybeToList $ Map.lookup v elbl
                , adjacent e e'
                ]
    pure (start, end, g)

elevation :: Parsec Void Text Elevation
elevation = Elevation <$> satisfy isAsciiLower <?> "Letter a-z"

poi :: Parsec Void Text POI
poi = (char 'S' $> S) <|> (char 'E' $> E)

adjacent :: Elevation -> Elevation -> Bool
adjacent u v = succ u >= v

surrounding :: (Int, Int) -> [(Int, Int)]
surrounding u = [f (d +) u | d <- [1, -1], f <- [first, second]]

-- >>> part1 <$> puzzleInput puzzle Example
-- 31
part1 :: (Ord a, G.StarSemiring e, Ord e) => (a, a, AdjacencyMap e a) -> e
part1 (start, end, g) = fromMaybe mempty $ shortestPath start end g

pathsFrom :: (Ord a, Ord e, G.Semiring e) => a -> G.AdjacencyMap e a -> [(a, e)]
pathsFrom src g = go (PQ.singleton src G.one ()) Set.empty
  where
    adj = G.adjacencyMap g
    successors u = maybe [] Map.toList $ Map.lookup u adj
    decrease u p' q = snd $ PQ.alter (\x -> ((), Just $ (p', ()) <> fromMaybe mempty x)) u q
    go (PQ.minView -> Just (u, pu, (), q)) s =
        let ss = filter ((`Set.notMember` s) . fst) $ successors u
            q' = foldl' (\qw (v, pv) -> decrease v (pu G.<.> pv) qw) q ss
            s' = Set.insert u s
         in (u, pu) : go q' s'
    go _ _ = []

shortestPath :: (Ord a, Ord e, G.Semiring e) => a -> a -> AdjacencyMap e a -> Maybe e
shortestPath start end g = fmap snd $ find ((end ==) . fst) $ pathsFrom start g
