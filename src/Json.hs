-- | TODO:
-- 1. Strings should support escaping.
-- 2. Proper error reporting.

module Json (parse) where

import Prelude
import Control.Applicative (Alternative(..), (<|>))
import Control.Monad ((<=<))
import Data.Char (isDigit)
import Data.Tuple (swap)

type Predicate a = a -> Bool

ensure :: Alternative f => Predicate a -> a -> f a
ensure p x = if p x then pure x else empty

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
    pure x = Parser $ Just . (, x)
    (Parser p) <*> (Parser q) = Parser $ \input -> do
        (output, f) <- p input
        (output', x) <- q output
        pure (output', f x)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p) <|> (Parser q) = Parser (\x -> p x <|> q x)

instance Monad Parser where
    p >>= f = Parser $ (\(y, a) -> runParser (f a) y) <=< runParser p

char :: Char -> Parser Char
char x = Parser $ \case
    y:ys | x == y -> Just (ys, y)
    _ -> Nothing

str :: String -> Parser String
str = traverse char

optional :: Monoid a => Parser a -> Parser a
optional x = x <|> Parser (Just . (, mempty))

while :: Predicate Char -> Parser String
while p = Parser $ ensure (not . null . snd) . swap . span p

optionalWhile :: Predicate Char -> Parser String
optionalWhile = optional . while

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep el = (:) <$> el <*> many (sep *> el) <|> pure []

ws :: Parser String
ws = optionalWhile (== ' ')

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ str "null"

jsonBool :: Parser JsonValue
jsonBool = (JsonBool True <$ str "true") <|> (JsonBool False <$ str "false")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> (double <|> int)
    where int = while isDigit
          double = (\x y z -> x <> [y] <> z) <$> int <*> char '.' <*> int

stringLiteral :: Parser String
stringLiteral = char '"' *> optionalWhile (/= '"') <* char '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (char '[' *> ws *> elements <* ws <* char ']')
    where elements = sepBy (ws *> char ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (char '{' *> ws *> elements <* ws <* char '}')
    where elements = sepBy (ws *> char ',' <* ws) pair
          pair = (\k _ v -> (k, v)) <$> stringLiteral <*> (ws *> char ':' <* ws) <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

complete :: (String, JsonValue) -> Maybe JsonValue
complete ("", xs) = Just xs
complete _ = Nothing

parse :: String -> Maybe JsonValue
parse = complete <=< runParser jsonValue

