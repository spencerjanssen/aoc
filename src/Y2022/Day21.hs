{-# LANGUAGE DeriveFunctor #-}

module Y2022.Day21 where

import AocUtil
import Data.Map.Lazy qualified as Map
import MegaParsecUtil
import Test.Tasty (TestTree)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (many, round, some)

puzzle :: Puzzle [(MonkeyName, Expr MonkeyName)]
puzzle =
    Puzzle
        { year = "2022"
        , day = "21"
        , parser = statements
        , parts = [Part (Map.lookup (MonkeyName "root") . solve) (Just 152) (Just 72664227897438)]
        }

test_ :: TestTree
test_ = tests puzzle

data O = Plus | Sub | Mul | Div
    deriving (Show)

newtype MonkeyName = MonkeyName Text
    deriving (Show, Eq, Ord)

data Expr a
    = C Int
    | Pure a
    | Oper (Expr a) O (Expr a)
    deriving (Functor, Show)

joinExpr :: Expr (Expr a) -> Expr a
joinExpr = \case
    Pure x -> x
    C n -> C n
    Oper e o f -> Oper (joinExpr e) o (joinExpr f)

instance Applicative Expr where
    pure = Pure
    f <*> x = f >>= \f' -> x >>= \x' -> pure $ f' x'

instance Monad Expr where
    m >>= f = joinExpr $ f <$> m

statements :: Parsec Void Text [(MonkeyName, Expr MonkeyName)]
statements = endBy statement eol <* eof

statement :: Parsec Void Text (MonkeyName, Expr MonkeyName)
statement = (,) <$> monkeyName <* ": " <*> expr

monkeyName :: Parsec Void Text MonkeyName
monkeyName = MonkeyName <$> asciiAlpha

expr :: Parsec Void Text (Expr MonkeyName)
expr = C <$> int <|> Oper <$> (Pure <$> monkeyName) <*> oper <*> (Pure <$> monkeyName)

oper :: Parsec Void Text O
oper =
    choice
        [ " + " $> Plus
        , " - " $> Sub
        , " * " $> Mul
        , " / " $> Div
        ]

eval :: Expr Int -> Int
eval = \case
    C n -> n
    Pure n -> n
    Oper x o y -> operFn o (eval x) (eval y)

operFn :: O -> Int -> Int -> Int
operFn = \case
    Plus -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> div

solve :: [(MonkeyName, Expr MonkeyName)] -> Map MonkeyName Int
solve es = m
  where
    m = Map.fromList $ map (fmap (\e -> eval $ (m Map.!) <$> e)) es
