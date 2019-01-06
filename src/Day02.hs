module Day02 where

import           Control.Monad.Trans
import           Data.List                      ( foldl' )

data Dir   = U | D | L | R deriving Show
type Instr = [Dir]

parseDir :: Char -> Dir
parseDir 'U' = U
parseDir 'D' = D
parseDir 'L' = L
parseDir 'R' = R
parseDir c   = error $ "Bad dir: " ++ [c]

nextA :: Char -> Dir -> Char
nextA '1' D = '4'
nextA '1' R = '2'
nextA '2' L = '1'
nextA '2' R = '3'
nextA '2' D = '5'
nextA '3' L = '2'
nextA '3' D = '6'
nextA '4' U = '1'
nextA '4' R = '5'
nextA '4' D = '7'
nextA '5' U = '2'
nextA '5' R = '6'
nextA '5' D = '8'
nextA '5' L = '4'
nextA '6' U = '3'
nextA '6' D = '9'
nextA '6' L = '5'
nextA '7' U = '4'
nextA '7' R = '8'
nextA '8' U = '5'
nextA '8' R = '9'
nextA '8' L = '7'
nextA '9' U = '6'
nextA '9' L = '8'
nextA c   _ = c

nextB :: Char -> Dir -> Char
nextB '1' D = '3'
nextB '2' R = '3'
nextB '2' D = '6'
nextB '3' U = '1'
nextB '3' R = '4'
nextB '3' D = '7'
nextB '3' L = '2'
nextB '4' D = '8'
nextB '4' L = '3'
nextB '5' R = '6'
nextB '6' U = '2'
nextB '6' R = '7'
nextB '6' D = 'A'
nextB '6' L = '5'
nextB '7' U = '3'
nextB '7' R = '8'
nextB '7' D = 'B'
nextB '7' L = '6'
nextB '8' U = '4'
nextB '8' R = '9'
nextB '8' D = 'C'
nextB '8' L = '7'
nextB '9' L = '8'
nextB 'A' U = '6'
nextB 'A' R = 'B'
nextB 'B' U = '7'
nextB 'B' R = 'C'
nextB 'B' D = 'D'
nextB 'B' L = 'A'
nextB 'C' U = '8'
nextB 'C' L = 'B'
nextB 'D' U = 'B'
nextB c   _ = c

runInstrA :: Char -> Instr -> Char
runInstrA c i = foldl' (\acc d -> nextA acc d) c i

runInstrAs :: [Instr] -> [Char]
runInstrAs is =
  drop 1 $ foldl' (\acc i -> acc ++ [runInstrA (last acc) i]) ['5'] is

runInstrB :: Char -> Instr -> Char
runInstrB c i = foldl' (\acc d -> nextB acc d) c i

runInstrBs :: [Instr] -> [Char]
runInstrBs is =
  drop 1 $ foldl' (\acc i -> acc ++ [runInstrB (last acc) i]) ['5'] is

parseInput :: MonadIO m => String -> m ([Instr])
parseInput f = do
  input <- liftIO . readFile $ f
  return $ (map . map) parseDir $ lines input

solve :: MonadIO m => [Instr] -> m ()
solve instrs = do
  liftIO . print . runInstrAs $ instrs
  liftIO . print . runInstrBs $ instrs

main :: IO ()
main = parseInput "input/02.txt" >>= solve
