module Y2021.Day16 where

import Data.Bits
import Data.FileEmbed
import Data.Semigroup
import Numeric (readHex)
import Test.Tasty.HUnit

example :: Text
example = decodeUtf8 $(embedFile "inputs/2021/day16/example.txt")

-- >>> parsedExample
-- [False,True,True,True,False,True,True,True,False,False,False,False,False,False,False,False,True,False,True,True,False,False,True,False,False,False,False,False,False,False,True,True,False,False,False,True,False,True,False,False,True,True,False,False,False,False,False,False,False,True,True,False,False,False,False,False]
parsedExample :: [Bool]
parsedExample = parse example

problem :: Text
problem = decodeUtf8 $(embedFile "inputs/2021/day16/problem.txt")

parsedProblem :: [Bool]
parsedProblem = parse problem

-- >>> parse "EE"
parse :: Text -> [Bool]
parse = concatMap bits4 . mapMaybe (mreadHex . pure) . toString
  where
    bits4 :: Word8 -> [Bool]
    bits4 n = map (testBit n) [3, 2 .. 0]
    mreadHex s = case readHex s of
        [(n, "")] -> Just n
        _ -> Nothing

bitString :: Text -> [Bool]
bitString = map (== '1') . toString

newtype BitsM a = BitsM {getBitsM :: State [Bool] a}
    deriving newtype (Functor, Applicative, Monad, MonadState [Bool])

evalBitsM :: HasCallStack => BitsM a -> [Bool] -> a
evalBitsM = evalState . getBitsM

bail :: HasCallStack => Text -> BitsM a
bail = error

eof :: HasCallStack => BitsM Bool
eof = gets null

region :: HasCallStack => Int -> BitsM a -> BitsM a
region n pgm = do
    bs <- get
    let (r, bs') = splitAt n bs
    if length r /= n
        then bail "not enough input for region"
        else do
            put bs'
            pure $! evalBitsM pgm r

abit :: HasCallStack => BitsM Bool
abit = do
    bs <- get
    case bs of
        [] -> bail "no bits left"
        (b : bs') -> put bs' $> b

nbits :: (HasCallStack, Bits a) => Int -> BitsM a
nbits n = bigEndian <$> replicateM n abit

-- >>> bigEndian $ bitString "011"
-- 3
bigEndian :: Bits a => [Bool] -> a
bigEndian = foldl' (.|.) zeroBits . zipWith (\i b -> if b then bit i else zeroBits) [0 ..] . reverse

data Operation
    = OSum
    | OProduct
    | OMinimum
    | OMaximum
    | OGreaterThan
    | OLessThan
    | OEqualTo
    deriving (Eq, Show)

data Packet
    = Literal Version Integer
    | Packets Version Operation [Packet]
    deriving (Eq, Show)

pfold :: (Version -> Integer -> a) -> (Version -> Operation -> [a] -> a) -> Packet -> a
pfold l o = go
  where
    go (Literal v i) = l v i
    go (Packets v op vs) = o v op $ map go vs

type Version = Word8
type TypeID = Word8

header :: BitsM (Version, TypeID)
header = liftA2 (,) (nbits 3) (nbits 3)

-- >>> evalBitsM packet $ bitString "110100101111111000101000"
-- Literal 6 2021
packet :: HasCallStack => BitsM Packet
packet = do
    (version, typeId) <- header
    case typeId of
        4 -> Literal version <$> literal
        _ -> do
            isPacketCountOperator <- abit
            let !op = case typeId of
                    0 -> OSum
                    1 -> OProduct
                    2 -> OMinimum
                    3 -> OMaximum
                    5 -> OGreaterThan
                    6 -> OLessThan
                    7 -> OEqualTo
                    _ -> error "unknown operator"
            Packets version op
                <$> if isPacketCountOperator
                    then do
                        subpackets <- nbits @Int 11
                        replicateM subpackets packet
                    else do
                        rsize <- nbits @Int 15
                        let untilEof = do
                                isEof <- eof
                                if isEof
                                    then pure []
                                    else liftA2 (:) packet untilEof
                        region rsize untilEof

literal :: BitsM Integer
literal = go 0
  where
    go !acc = do
        (moreInput, x) <- lgroup
        let acc' = shiftL acc 4 .|. x
        if moreInput then go acc' else pure acc'

-- do we need Integer here?
lgroup :: HasCallStack => BitsM (Bool, Integer)
lgroup = liftA2 (,) abit (nbits 4)

-- >>> part1 parsedExample
-- 31
part1 :: [Bool] -> Int
part1 bs = getSum $ pfold (\v _ -> Sum $ fromIntegral v) (\v _ vs -> mconcat $ Sum (fromIntegral v) : vs) $ evalBitsM packet bs

-- >>> part2 parsedExample
-- 54
part2 :: [Bool] -> Int
part2 bs = pfold (\_ n -> fromIntegral n) op $ evalBitsM packet bs
  where
    op _ OGreaterThan [x, y] = fromEnum $ x > y
    op _ OLessThan [x, y] = fromEnum $ x < y
    op _ OEqualTo [x, y] = fromEnum $ x == y
    op _ OSum xs = monoidally Sum getSum xs
    op _ OProduct xs = monoidally Product getProduct xs
    op _ OMaximum xs = monoidally Max getMax xs
    op _ OMinimum xs = monoidally Min getMin xs
    op _ _ _ = error "unknown operator or invalid args"

monoidally :: Monoid (f a) => (a -> f a) -> (f a -> a) -> [a] -> a
monoidally to out = out . foldMap to

unit_literal_packet :: Assertion
unit_literal_packet = evalBitsM packet (bitString "110100101111111000101000") @?= Literal 6 2021

unit_length_type_0 :: Assertion
unit_length_type_0 = evalBitsM packet (parse "38006F45291200") @?= Packets 1 OLessThan [Literal 6 10, Literal 2 20]

unit_length_type_1 :: Assertion
unit_length_type_1 = evalBitsM packet (parse "EE00D40C823060") @?= Packets 7 OMaximum [Literal 2 1, Literal 4 2, Literal 1 3]

unit_part1_example :: Assertion
unit_part1_example = part1 parsedExample @?= 31

unit_part1_problem :: Assertion
unit_part1_problem = part1 parsedProblem @?= 977

unit_part2_example :: Assertion
unit_part2_example = part2 parsedExample @?= 54

unit_part2_problem :: Assertion
unit_part2_problem = part2 parsedProblem @?= 101501020883
