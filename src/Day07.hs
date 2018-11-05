{-# LANGUAGE LambdaCase #-}
module Day07 where

import           Data.List
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Element =
  Brackets String
  | NoBrackets String
  deriving Show

newtype Address =
  Address [Element] deriving Show

type Parser = Parsec Void String

addressParser :: Parser Address
addressParser = Address <$> many (brackets <|> noBrackets)
 where
  brackets   = Brackets <$> between (char '[') (char ']') (many letterChar)
  noBrackets = NoBrackets <$> some letterChar

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> return x

abba :: String -> Bool
abba str@(a : b : c : d : _) = a == d && b == c && a /= b || abba (tail str)
abba _                       = False

aba :: String -> Bool
aba str@(a : b : c : _) = a == c && a /= b || abba (tail str)
aba _                   = False

abbaInBrackets :: Element -> Bool
abbaInBrackets = \case
  Brackets   str -> abba str
  NoBrackets _   -> False

abbaInNoBrackets :: Element -> Bool
abbaInNoBrackets = \case
  Brackets   _   -> False
  NoBrackets str -> abba str

tls :: Address -> Bool
tls (Address elements) =
  any abbaInNoBrackets elements && not (any abbaInBrackets elements)

abasStr :: String -> [String]
abasStr str = go str []
 where
  go (a : b : c : rest) abas =
    go (b : c : rest) $ if aba [a, b, c] then [a, b, c] : abas else abas
  go _ abas = abas

abasEls :: [Element] -> [String]
abasEls elements = catMaybes (noBracketString <$> elements) >>= abasStr
 where
  noBracketString = \case
    Brackets   _   -> Nothing
    NoBrackets str -> Just str

babsEls :: [Element] -> [String] -> [String]
babsEls elements abas = abas `intersect` (abaIfy <$> potentialBabs)
 where
  abaIfy (a : b : _ : _) = [b, a, b]
  abaIfy str             = str
  potentialBabs = catMaybes (bracketString <$> elements) >>= abasStr
  bracketString = \case
    Brackets   str -> Just str
    NoBrackets _   -> Nothing

ssl :: Address -> Bool
ssl (Address elements) = not . null $ babsEls elements (abasEls elements)

main :: IO ()
main = do
  addresses <- parseLines addressParser "input/07.txt"
  print . length . filter (== True) $ tls <$> addresses
  print . length . filter (== True) $ ssl <$> addresses
