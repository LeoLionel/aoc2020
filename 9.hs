
import Data.List
import Data.Maybe

main :: IO ()
main = do
  input <- map (\x -> read x :: Int) . lines <$> readFile "input9"
  print $ findFirst input
  print $ solve2    input


type Header = [Int]

validBy :: Int -> Header -> Bool
validBy x xs = not . null $ [ y + z | y <- xs, z <- xs, y+z == x, y /= z ]


findFirst :: [Int] -> Int
findFirst xs =
  let (header, num:_) = splitAt 25 xs
  in if num `validBy` header
       then findFirst (tail xs)
       else num


--part 2

-- contiguous subsequences of length at least 2
contSubsequences :: [a] -> [[a]]
contSubsequences = concatMap (init . init . tails) . drop 2 . inits 


findRange :: [Int] -> Int -> [Int]
findRange xs n =
  let ranges = filter (\x -> sum x == n ) $ contSubsequences xs 
  in case ranges of
    r:_       -> r
    otherwise -> error "faulty puzzle"


solve2 :: [Int] -> Int
solve2 xs =
  let w = findFirst xs
      r = findRange xs w
  in minimum r + maximum r
  

testInput :: [Int]
testInput = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]
