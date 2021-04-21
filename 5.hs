
import Data.List

main :: IO ()
main = do
  input <- lines <$> readFile "input5"
  print $ solve  input
  print $ solve2 input


rowNo :: String -> Int
rowNo = digitsToBinaryNum . map f . take 7
  where
    f 'F' = 0
    f 'B' = 1


colNo :: String -> Int
colNo = digitsToBinaryNum . map f . drop 7
  where
    f 'R' = 1
    f 'L' = 0


digitsToBinaryNum :: [Int] -> Int
digitsToBinaryNum = foldl1' (\x y -> 2*x + y)


seatID :: String -> Int
seatID l = (rowNo l) * 8 + (colNo l)


solve :: [String] -> Int
solve = maximum . map seatID


-- part 2

solve2 :: [String] -> Int
solve2 = mySeat . map seatID


mySeat :: [Int] -> Int
mySeat xs =
  let low  = minimum xs
      high = maximum xs
  in head [ id | id <- [low..high] , id `notElem` xs ]
