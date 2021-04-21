import Data.List
import Numeric

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

main :: IO ()
main = do
  input <- return (13135480, 8821721)
  print $ getEncrKey input




transform :: Integer -> Integer -> Integer
transform subjNum loopSize = (subjNum ^ loopSize) `rem` 20201227

-- too slow
findLoopSize :: Integer -> Maybe Int
findLoopSize pubKey =
  elemIndex pubKey $ map (transform 7) [0,1..20201225]



-- computes x such that g^x `rem` p == h, for p prime
-- https://en.wikipedia.org/wiki/Baby-step_giant-step

discreteLog :: Integer -> Integer -> Integer -> Integer
discreteLog g h p = go 0 h 
  where
    n = p - 1
    m = ceiling . sqrt . fromInteger $ n
    babySteps = Map.fromList [ (g ^ j `rem` p, j) | j <- [0 .. m-1] ]
    gIm       = g^(n-m) `rem` p

    go i acc = if Map.member acc babySteps
                  then let Just j = Map.lookup acc babySteps
                       in (m*i + j)
                  else let acc' = acc * gIm `rem` p
                       in acc' `seq` go (i+1) acc' 

getEncrKey (pubKeyA, pubKeyB) = transform pubKeyA loopSizeB
  where
    loopSizeB = discreteLog 7 pubKeyB 20201227

