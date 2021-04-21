
import Data.List
import Data.List.Split


main :: IO ()
main = do
  input <- lines <$> readFile "input6"
  print $ solve  input
  print $ solve2 input


groupInput :: [String] -> [[String]]
groupInput = splitOn [""]


solve :: [String] -> Int
solve =  sum . map length . map anyoneYes . groupInput
  where   
    anyoneYes :: [String] -> String
    anyoneYes = nub . concat


solve2 :: [String] -> Int
solve2 =  sum . map length . map everyoneYes . groupInput
  where 
    everyoneYes :: [String] -> String
    everyoneYes = foldl1' intersect
    

testInput :: [String]
testInput =
  ["abc","","a","b","c","","ab","ac","","a","a","a","a","","b"]
