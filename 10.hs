
import Data.List


main :: IO ()
main = do
  input <- map (\x -> read x :: Int) . lines <$> readFile "input10"
  print $ solve  input  
  print $ solve2 input


diffs :: Num a => [a] -> [a]
diffs [] = []
diffs (x:xs) = zipWith (-) (xs) (x:xs)


solve xs = (count 1 differences) * (count 3 differences)
  where
    voltages = [0] ++ sort xs ++ [maximum xs + 3]
    differences = diffs voltages
    count x = length . filter (==x)


testInput :: [Int]
testInput =
  [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19
  ,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]



-- part 2 


diffsWithFirst :: Num a => [a] -> [a]
diffsWithFirst [] = []
diffsWithFirst (x:xs) = x : zipWith (-) (xs) (x:xs)


splitAtIndices :: [Int] -> [a] -> [[a]]
{- splitAtIndices generalizes the standard function splitAt
   splitAt 4 [1..10] == ([1,2,3,4], [5,6,7,8,9,10])
   splitAtIndices [4,7] [1..10] == [[1,2,3,4],[5,6,7],[8,9,10]]   
-}
splitAtIndices [] xs = [xs]
splitAtIndices is xs = go differences xs [] 
  where
    differences = diffsWithFirst . sort $ is

    go []  xs acc = reverse $ acc
    go dss xs acc =
      let (d:ds) = dss
          (h, rest) = splitAt d xs
      in case ds of
           []        -> reverse $ rest : h : acc
           otherwise -> go ds rest (h:acc)


splitAtJumpOf3 :: [Int] -> [[Int]]
splitAtJumpOf3 xs = splitAtIndices is xs
  where
    is = elemIndices 3 . diffsWithFirst $ xs


countPossibleGroups :: [Int] -> Int
countPossibleGroups xs
  | length xs < 3 = 1
  | otherwise = length $ filter validArrangement candidates
    where     
      candidates = [ [start] ++ ys ++ [end] | ys <- powerSet middle ]
      start  = head xs
      end    = last xs
      middle = tail . init $ xs
      
      validArrangement :: [Int] -> Bool
      validArrangement xs = all (<4) (diffs xs)

      powerSet :: [a] -> [[a]]
      powerSet [] = [[]]
      powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)


solve2 :: [Int] -> Int
solve2 xs = product . map countPossibleGroups . splitAtJumpOf3 $ voltages
  where
    voltages = [0] ++ sort xs ++ [maximum xs + 3]


testInput2 :: [Int]
testInput2 = [1,4,5,6,7,10,11,12,15,16,19]
