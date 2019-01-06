module Day09 where

import           Control.Monad
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

noMarkerParser :: Parser Int
noMarkerParser = length <$> some (satisfy (/= '('))

lengthRepsParser :: Parser (Int, Int)
lengthRepsParser = do
  char '('
  repLength <- decimal
  char 'x'
  numReps <- decimal
  char ')'
  pure (repLength, numReps)

marker1Parser :: Parser Int
marker1Parser = do
  (repLength, numReps) <- lengthRepsParser
  replicateM_ repLength anyChar
  pure $ numReps * repLength

marker2Parser :: Parser Int
marker2Parser = do
  (repLength, numReps) <- lengthRepsParser
  substr               <- replicateM repLength anyChar
  let subResult = case parse part2Parser mempty substr of
        Left  err -> error $ parseErrorPretty err
        Right x   -> x
  pure $ numReps * subResult

part1Parser :: Parser Int
part1Parser = sum <$> many (marker1Parser <|> noMarkerParser)

part2Parser :: Parser Int
part2Parser = sum <$> many (marker2Parser <|> noMarkerParser)

decompressInput :: Parser Int -> String -> Int
decompressInput partParser input =
  case parse (partParser <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> x

main :: IO ()
main = do
  input <- init <$> readFile "input/09.txt"
  print $ decompressInput part1Parser input
  print $ decompressInput part2Parser input
