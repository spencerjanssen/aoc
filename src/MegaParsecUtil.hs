module MegaParsecUtil (
    int,
    parseThrow,
) where

import Data.Char (isDigit)
import Text.Megaparsec

parseThrow :: Parsec Void Text a -> String -> Text -> a
parseThrow p desc input = either (error . fromString . errorBundlePretty) id $ parse p desc input

int :: Parsec Void Text Int
int = maybe (fail "invalid number") pure . (readMaybe . toString) =<< takeWhile1P (Just "digit") isDigit
