module Day04 where

import Control.Monad.Trans
import Data.List           (group, sort, sortBy)
import Data.List.Split     (splitOn)
import Data.Ord            (comparing)

data Room = Room String Int String deriving Show

parseRoom :: String -> Room
parseRoom s = Room s' i ck
  where
    lst = splitOn "-" s
    s'  = concat $ take (length lst - 1) lst
    e   = splitOn "[" . init . last $ lst
    i   = read (e !! 0) :: Int
    ck  = e !! 1

parseInput :: MonadIO m => String -> m ([Room])
parseInput f = do
  input <- liftIO . readFile $ f
  return . map parseRoom . lines $ input

valid :: Room -> Bool
valid (Room s _ ck) = ck == ck'
  where
    freqs  = map (\x -> (length x, [head x])) . group . sort $ s
    cmp    = flip (comparing fst) `mappend` comparing snd
    sorted = sortBy cmp freqs
    ck'    = concatMap snd . take 5 $ sorted

shiftStr :: Int -> String -> String
shiftStr n = map shiftChar
  where
    alphabet = ['a'..'z']
    shiftChar c = head
        $ drop (length alphabet + n)
        $ dropWhile (/= c)
        $ cycle alphabet

solve :: MonadIO m => [Room] -> m ()
solve rooms = do
  let validRooms       = filter valid rooms
      sectorIds        = map (\(Room _ i _) -> i) validRooms
      nps              = "northpoleobjectstorage"
      (Room _ npId _)  = filter (\(Room s i _) -> nps == shiftStr i s) validRooms !! 0
  liftIO . print . sum $ sectorIds
  liftIO . print $ npId

main :: IO ()
main = parseInput "input/04.txt" >>= solve
