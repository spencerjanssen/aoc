module MegaParsecUtil (
    int,
    digit,
    parseThrow,
) where

import Data.Char (isDigit)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)

parseThrow :: Parsec Void Text a -> String -> Text -> a
parseThrow p desc input = either (error . fromString . errorBundlePretty) id $ parse p desc input

digit :: Parsec Void Text Int
digit = maybe (fail "invalid digit") pure . (readMaybe . pure) =<< digitChar

int :: Parsec Void Text Int
int = maybe (fail "invalid number") pure . (readMaybe . toString) =<< takeWhile1P (Just "digit") isDigit
