module Day06 where

import           Data.List                      ( transpose
                                                , foldl'
                                                , maximumBy
                                                , minimumBy
                                                )
import qualified Data.Map                      as Map
import           Data.Ord                       ( comparing )


freqs :: String -> [(Char, Int)]
freqs str =
  Map.toList $ foldl' (\acc c -> Map.insertWith (+) c 1 acc) Map.empty str

maxFreq :: String -> Char
maxFreq str = fst $ maximumBy (comparing snd) (freqs str)

findFreq
  :: (((Char, Int) -> (Char, Int) -> Ordering) -> [(Char, Int)] -> (Char, Int))
  -> String
  -> Char
findFreq ordering str = fst $ ordering (comparing snd) (freqs str)

main :: IO ()
main = do
  raw <- readFile "input/06.txt"
  let rows    = lines raw
      columns = transpose rows
  print $ findFreq maximumBy <$> columns
  print $ findFreq minimumBy <$> columns
