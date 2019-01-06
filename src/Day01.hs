module Day01 where

import           Control.Monad.Trans
import           Data.List                      ( foldl' )
import           Data.List.Split                ( splitOn )

data Dir   = N | S | E | W
data Turn  = R | L
data Instr = Instr Turn Int
data Coord = Coord Int Int deriving Eq
data State = State Coord Dir [Coord]

parseInstr :: String -> Instr
parseInstr s = Instr d a
 where
  a = read . tail $ s :: Int
  d = case head s of
    'R' -> R
    'L' -> L
    _   -> error $ "Invalid instr: " ++ s

parseInput :: MonadIO m => String -> m ([Instr])
parseInput f = do
  input <- liftIO . readFile $ f
  return $ map parseInstr (splitOn ", " . init $ input)

rotate :: Dir -> Turn -> Dir
rotate N R = E
rotate N L = W
rotate S R = W
rotate S L = E
rotate E R = S
rotate E L = N
rotate W R = N
rotate W L = S

nextCoord :: Coord -> Dir -> Int -> Coord
nextCoord (Coord x y) N i = Coord x (y - i)
nextCoord (Coord x y) S i = Coord x (y + i)
nextCoord (Coord x y) E i = Coord (x + i) y
nextCoord (Coord x y) W i = Coord (x - i) y

manhattan :: Coord -> Int
manhattan (Coord x y) = abs x + abs y

step :: State -> Instr -> State
step (State c d cs) (Instr t i) = State c' d' cs'
 where
  d'  = rotate d t
  c'  = nextCoord c d' i
  cs' = cs ++ (map (\x -> nextCoord c d' x) [1 .. i])

firstDup :: [Coord] -> Maybe Int
firstDup cs = go 1
 where
  go i | i >= length cs = Nothing
       | found          = Just $ manhattan c
       | otherwise      = go $ i + 1
   where
    c     = cs !! i
    cs'   = take i cs
    found = elem c cs'

solve :: MonadIO m => [Instr] -> m ()
solve instrs = do
  let startC       = (Coord 0 0)
      start        = State startC N [startC]
      State c _ cs = foldl' (\acc i -> step acc i) start instrs
  liftIO . print . manhattan $ c
  liftIO . print . firstDup $ cs

main :: IO ()
main = parseInput "input/01.txt" >>= solve
