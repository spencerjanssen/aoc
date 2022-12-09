module AocUtil where

import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Text qualified as T
import MegaParsecUtil
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

data Input = Example | Problem
    deriving (Show)

data Puzzle a = Puzzle
    { year :: Text
    , day :: Text
    , parser :: Parsec Void Text a
    , parts :: [Part a]
    }

data Part a = Part
    { solver :: a -> Int
    , exampleSolution :: Int
    , problemSolution :: Int
    }

puzzleInput :: Puzzle a -> Input -> IO a
puzzleInput Puzzle{year, day, parser} i = parseThrowIO parser filePath . decodeUtf8 =<< BS.readFile filePath
  where
    filePath = "inputs" </> T.unpack year </> "day" <> T.unpack day </> map toLower (show i) <.> "txt"

tests :: Puzzle a -> TestTree
tests p@Puzzle{parts, day} = withResource getInputs cleanup $ \gi ->
    testGroup ("Day " <> toString day) (zipWith (testPart gi) [1 :: Int ..] parts)
  where
    getInputs = liftA2 (,) (puzzleInput p Example) (puzzleInput p Problem)
    cleanup _ = pure ()
    testPart gi i (Part{solver, exampleSolution, problemSolution}) =
        testGroup
            ("Part " <> show i)
            [ testCase "Example" $ do
                (ex, _) <- gi
                solver ex @?= exampleSolution
            , testCase "Problem" $ do
                (_, pr) <- gi
                solver pr @?= problemSolution
            ]
