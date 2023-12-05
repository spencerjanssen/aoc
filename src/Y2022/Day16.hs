module Y2022.Day16 where

import AocUtil
import Test.Tasty

data G = G

puzzle :: Puzzle G
puzzle =
    Puzzle
        { year = "2022"
        , day = "16"
        , parser = pure G
        , parts = [Part part1 1651 1]
        }

tests_ :: TestTree
tests_ = tests puzzle

part1 :: G -> Int
part1 = const $ -1
