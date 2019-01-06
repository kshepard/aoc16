{-# LANGUAGE LambdaCase #-}
module Day08 where

import           Data.Foldable
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

newtype Display a = Display ((Int, Int) -> a)

data Instr =
    Rect Int Int
  | RotateCol Int Int
  | RotateRow Int Int
  deriving Show

rows, cols :: Int
rows = 6
cols = 50

emptyDisplay :: Display Bool
emptyDisplay = Display $ const False

instrParser :: Parser Instr
instrParser =
  Rect
    <$> (string "rect " *> decimal)
    <*> (char 'x' *> decimal)
    <|> RotateCol
    <$> (string "rotate column x=" *> decimal)
    <*> (string " by " *> decimal)
    <|> RotateRow
    <$> (string "rotate row y=" *> decimal)
    <*> (string " by " *> decimal)

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> return x

printDisplay :: Display Bool -> IO ()
printDisplay (Display display) = for_ [0 .. rows - 1] $ \row -> do
  for_ [0 .. cols - 1]
    $ \col -> putStr $ if display (row, col) then "*" else " "
  print ""

wrap :: Int -> Int -> Int
wrap val maxVal =
  if val < 0 then wrap (val + maxVal) maxVal else val `mod` maxVal

updateDisplay :: Display Bool -> Instr -> Display Bool
updateDisplay (Display display) = \case
  Rect      w h -> Display $ \(r, c) -> (c < w && r < h) || display (r, c)
  RotateCol c n -> Display $ \(r, c') ->
    if c == c' then display (wrap (r - n) rows, c') else display (r, c')
  RotateRow r n -> Display $ \(r', c) ->
    if r == r' then display (r', wrap (c - n) cols) else display (r', c)

main :: IO ()
main = do
  instrs <- parseLines instrParser "input/08.txt"
  let finalDisplay@(Display display) = foldl' updateDisplay emptyDisplay instrs
  print
    .   sum
    $   fromEnum
    .   display
    <$> ((,) <$> [0 .. rows - 1] <*> [0 .. cols - 1])
  printDisplay finalDisplay
