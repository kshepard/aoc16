module Day03 where

import           Control.Monad.Trans
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )

data Triangle = Triangle Int Int Int

parseInput :: MonadIO m => String -> m ([Triangle])
parseInput f = do
  input <- liftIO . readFile $ f
  return $ map (\t -> Triangle (t !! 0) (t !! 1) (t !! 2)) $ chunksOf
    3
    [ read x :: Int | x <- concatMap (splitOn " ") (lines input), x /= "" ]

valid :: Triangle -> Bool
valid (Triangle a b c) = a + b > c && a + c > b && b + c > a

reconfigure3 :: [Triangle] -> [Triangle]
reconfigure3 [Triangle a1 b1 c1, Triangle a2 b2 c2, Triangle a3 b3 c3] =
  [Triangle a1 a2 a3, Triangle b1 b2 b3, Triangle c1 c2 c3]
reconfigure3 _ = error "Must supply 3 triangles"

getVerticles :: [Triangle] -> [Triangle]
getVerticles = concatMap reconfigure3 . chunksOf 3

solve :: MonadIO m => [Triangle] -> m ()
solve triangles = do
  liftIO . print . length $ filter valid triangles
  liftIO . print . length . filter valid $ getVerticles triangles

main :: IO ()
main = parseInput "input/03.txt" >>= solve
