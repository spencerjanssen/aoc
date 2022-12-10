module MegaParsecUtil (
    int,
    digit,
    parseThrow,
    parseThrowIO,
    parseEither,
    asciiAlphaChar,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
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

int :: Parsec Void Text Int
int = maybe (fail "invalid number") pure . (readMaybe . toString) =<< takeWhile1P (Just "digit") isDigit

asciiAlphaChar :: Parsec Void Text Char
asciiAlphaChar = satisfy (\c -> isAsciiLower c || isAsciiUpper c) <?> "ASCII alphabetical char"
