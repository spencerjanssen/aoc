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
        , parts =
            [ Part (Map.lookup "root" . solve) (Just 152) (Just 72664227897438)
            , Part (findZero . equation) 301 3916491093817
            ]
        }

test_ :: TestTree
test_ = tests puzzle

data O = Plus | Sub | Mul | Div
    deriving (Show, Eq)

newtype MonkeyName = MonkeyName Text
    deriving (Show, Eq, Ord, IsString)

data Expr a
    = C Rational
    | Pure a
    | Oper (Expr a) O (Expr a)
    deriving (Functor, Show, Eq)

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
expr = C . fromIntegral <$> int <|> Oper <$> (Pure <$> monkeyName) <*> oper <*> (Pure <$> monkeyName)

oper :: Parsec Void Text O
oper =
    choice
        [ " + " $> Plus
        , " - " $> Sub
        , " * " $> Mul
        , " / " $> Div
        ]

efold :: (Rational -> b) -> (a -> b) -> (O -> b -> b -> b) -> Expr a -> b
efold c p op = go
  where
    go = \case
        C n -> c n
        Pure x -> p x
        Oper x o y -> op o (go x) (go y)

eval :: Expr Rational -> Rational
eval = efold id id operFn

operFn :: O -> Rational -> Rational -> Rational
operFn = \case
    Plus -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> (/)

solve :: [(MonkeyName, Expr MonkeyName)] -> Map MonkeyName Rational
solve es = m
  where
    m = Map.fromList $ map (fmap (\e -> eval $ (m Map.!) <$> e)) es

data Humn = Humn
    deriving (Show, Eq)

equation :: [(MonkeyName, Expr MonkeyName)] -> Expr Humn
equation es = case m Map.! "root" of
    Oper x _ y -> Oper x Sub y
    _ -> error "root is not an operator"
  where
    m = Map.fromList $ map f es
    f (mn, e) =
        ( mn
        , case mn of
            "humn" -> Pure Humn
            _ -> (m Map.!) =<< e
        )

simplify :: Expr a -> Expr a
simplify = efold C Pure f
  where
    f o (C x) (C y) = C $ operFn o x y
    f o x y = Oper x o y

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f x
    | x' == f x = x
    | otherwise = fixpoint f x'
  where
    x' = f x

derivative :: Expr a -> Expr a
derivative = \case
    Oper x Plus y -> Oper (derivative x) Plus (derivative y)
    Oper x Sub y -> Oper (derivative x) Sub (derivative y)
    Oper f Mul g ->
        let f' = derivative f
            g' = derivative g
         in (f' `emul` g) `eplus` (f `emul` g')
    Oper f Div g ->
        let f' = derivative f
            g' = derivative g
         in ((f' `emul` g) `eplus` (f `emul` g')) `ediv` (g `emul` g)
    Pure x -> Pure x
    C _ -> C 0
  where
    emul f = Oper f Mul
    eplus f = Oper f Plus
    ediv f = Oper f Div

findZero :: Expr Humn -> Rational
findZero ex = go 1
  where
    f x = eval $ ex <&> \Humn -> x
    dexp = derivative ex
    f' x = eval $ dexp <&> \Humn -> x
    approx x = x - f x / f' x
    go x
        | f x == 0 = x
        | otherwise = go $ approx x
