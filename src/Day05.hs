module Day05 where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as B
import           Crypto.Hash.MD5                ( hash )
import           Data.Char                      ( digitToInt )
import           Data.Monoid                    ( (<>) )
import           Data.ByteString.Base16         ( encode )
import           Data.List                      ( isInfixOf )

mkHash :: B.ByteString -> Int -> B.ByteString
mkHash doorId index = encode hashed
  where hashed = hash $ doorId <> B.pack (show index)

fiveZeros :: B.ByteString
fiveZeros = B.pack "00000"

isValid :: B.ByteString -> Bool
isValid = BS.isPrefixOf fiveZeros

part1 :: B.ByteString -> String
part1 doorId = passwd
 where
  hashes = take 8 . filter isValid $ mkHash doorId <$> [0 ..]
  passwd = (\bs -> B.unpack bs !! 5) <$> hashes

part2 :: B.ByteString -> String
part2 doorId = go (replicate 8 ' ') 0
 where
  go pwd ind | not $ " " `isInfixOf` pwd = pwd
             | otherwise                 = go newPwd (ind + 1)
   where
    newHash    = mkHash doorId ind
    newHashStr = B.unpack newHash
    newPos     = newHashStr !! 5
    goodPos    = newPos `elem` "01234567" && pwd !! intPos == ' '
    intPos     = digitToInt newPos
    newChar    = newHashStr !! 6
    newPwd     = if isValid newHash && goodPos
      then take intPos pwd ++ [newChar] ++ drop (intPos + 1) pwd
      else pwd

main :: IO ()
main = do
  raw <- BS.readFile "input/05.txt"
  let doorId = BS.init raw
  print $ part1 doorId
  print $ part2 doorId
