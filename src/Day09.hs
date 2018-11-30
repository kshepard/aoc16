module Day09 where

import           Control.Monad
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String


inputParser :: Parser Int -> Parser Int
inputParser markerParser = sum <$> many (markerParser <|> noMarkerParser)

markerParser1 :: Parser Int
markerParser1 = do
  char '('
  repLength <- decimal
  char 'x'
  numReps <- decimal
  char ')'
  replicateM_ repLength anyChar
  pure $ numReps * repLength

markerParser2 :: Parser Int
markerParser2 =
  -- TODO
  undefined

noMarkerParser :: Parser Int
noMarkerParser = length <$> some (satisfy (/= '('))

decompressInput :: Parser Int -> String -> Int
decompressInput markerParser input =
  case parse (inputParser markerParser <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> x


main :: IO ()
main = do
  input <- readFile "input/09.txt"
  let finalInput = init input
  print $ decompressInput markerParser1 finalInput
  print $ decompressInput markerParser2 finalInput
