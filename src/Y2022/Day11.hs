module Y2022.Day11 where

import AocUtil
import Data.Map.Strict qualified as Map
import MegaParsecUtil
import Test.Tasty
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (round)

puzzle :: Puzzle Monkeys
puzzle =
    Puzzle
        { year = "2022"
        , day = "11"
        , parser = monkeys
        , parts =
            [ Part (score . rounds 20 (`div` 3)) 10605 50172
            , Part part2 2713310158 11614682178
            ]
        }

test_ :: TestTree
test_ = tests puzzle

type MonkeyID = Int

data ExprVal = Old | Int Int
    deriving (Show)

data Operator = Add | Mul
    deriving (Show)

data Expr = Expr ExprVal Operator ExprVal
    deriving (Show)

type Monkeys = Map MonkeyID Monkey

data Monkey = Monkey
    { items :: Seq Int
    , operation :: Expr
    , divisibleBy :: Int
    , trueMonkey :: MonkeyID
    , falseMonkey :: MonkeyID
    , inspected :: Int
    }
    deriving (Show)

-- >>> puzzleInput puzzle Example
-- [(0,Monkey {items = [79,98], operation = Expr Old Mul (Int 19), divisibleBy = 23, trueMonkey = 2, falseMonkey = 3}),(1,Monkey {items = [54,65,75,74], operation = Expr Old Add (Int 6), divisibleBy = 19, trueMonkey = 2, falseMonkey = 0}),(2,Monkey {items = [79,60,97], operation = Expr Old Mul Old, divisibleBy = 13, trueMonkey = 1, falseMonkey = 3}),(3,Monkey {items = [74], operation = Expr Old Add (Int 3), divisibleBy = 17, trueMonkey = 0, falseMonkey = 1})]
monkeys :: Parsec Void Text Monkeys
monkeys = fromList <$> sepBy monkey eol <* eof

monkey :: ParsecT Void Text Identity (MonkeyID, Monkey)
monkey = do
    void "Monkey "
    monkeyID <- int
    void ":"
    void eol
    void "  Starting items: "
    items <- fromList <$> sepBy int ", "
    void eol
    void "  Operation: new = "
    operation <- expr
    void eol
    void "  Test: divisible by "
    divisibleBy <- int
    void eol
    void "    If true: throw to monkey "
    trueMonkey <- int
    void eol
    void "    If false: throw to monkey "
    falseMonkey <- int
    void eol
    pure (monkeyID, Monkey{inspected = 0, ..})
  where
    ident = ("old" $> Old) <|> (Int <$> int)
    op = (" + " $> Add) <|> (" * " $> Mul)
    expr = Expr <$> ident <*> op <*> ident

turn :: (Int -> Int) -> Monkey -> (Monkey, [(MonkeyID, Int)])
turn w m@Monkey{..} = (m{items = mempty, inspected = inspected + length items}, item <$> toList items)
  where
    item x =
        let x' = w $ evalExpr x operation
         in ( if mod x' divisibleBy == 0
                then trueMonkey
                else falseMonkey
            , x'
            )

toss :: Monkeys -> (MonkeyID, Int) -> Monkeys
toss ms (mid, x) = Map.adjust (\m -> m{items = items m <> pure x}) mid ms

round :: (Int -> Int) -> Monkeys -> Monkeys
round w ms0 = foldl' f ms0 $ Map.keys ms0
  where
    f ms mid = case Map.lookup mid ms of
        Nothing -> ms
        Just m0 ->
            let (m', ts) = turn w m0
             in foldl' toss (Map.insert mid m' ms) ts

rounds :: Int -> (Int -> Int) -> Monkeys -> Monkeys
rounds 0 _ ms = ms
rounds n w !ms = rounds (pred n) w $ round w ms

score :: Monkeys -> Int
score = product . take 2 . sortBy (flip compare) . map inspected . Map.elems

part2 :: Monkeys -> Int
part2 ms = score $ rounds 10000 (`mod` cd) ms
  where
    cd = product . ordNub . map divisibleBy $ Map.elems ms

evalExpr :: Int -> Expr -> Int
evalExpr old (Expr x op y) = f (evalVal x) (evalVal y)
  where
    evalVal = \case
        Old -> old
        Int n -> n
    f = case op of
        Add -> (+)
        Mul -> (*)
