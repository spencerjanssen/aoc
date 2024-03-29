module MegaParsecUtil (
    int,
    digit,
    parseThrow,
    parseThrowIO,
    parseEither,
    asciiAlphaChar,
    signedInt,
    alphaNum,
    alphaNumDot,
    asciiAlpha,
    nonNumericChar,
) where

import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)

parseThrow :: Parsec Void Text a -> String -> Text -> a
parseThrow p desc input = either (error . fromString) id $ parseEither p desc input

parseThrowIO :: Parsec Void Text a -> String -> Text -> IO a
parseThrowIO p desc input = either fail pure $ parseEither p desc input

parseEither :: Parsec Void Text a -> String -> Text -> Either String a
parseEither p desc input = first errorBundlePretty $ parse p desc input

digit :: Parsec Void Text Int
digit = maybe (fail "invalid digit") pure . (readMaybe . pure) =<< digitChar

signedInt :: Parsec Void Text Int
signedInt = maybe id (const negate) <$> optional (single '-') <*> int

int :: Parsec Void Text Int
int = maybe (fail "invalid number") pure . (readMaybe . toString) =<< takeWhile1P (Just "digit") isDigit

asciiAlpha :: Parsec Void Text Text
asciiAlpha = takeWhile1P (Just "ASCII alphabetical char") (liftA2 (||) isAsciiLower isAsciiUpper)

asciiAlphaChar :: Parsec Void Text Char
asciiAlphaChar = satisfy (\c -> isAsciiLower c || isAsciiUpper c) <?> "ASCII alphabetical char"

alphaNum :: Parsec Void Text Text
alphaNum = takeWhile1P (Just "ascii alpha num") isAlphaNum

alphaNumDot :: Parsec Void Text Text
alphaNumDot = takeWhile1P (Just "ascii alpha num or .") (liftA2 (||) isAlphaNum ('.' ==))

nonNumericChar :: Parsec Void Text Char
nonNumericChar = satisfy (\c -> not $ any ($ c) [isDigit, isControl, isSpace]) <?> "non-numeric char"
