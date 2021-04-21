
main :: IO ()
main = do
  input <- lines <$> readFile "input1" 

  let numbers = map (read :: String -> Int) input

  print . head $ solve  numbers
  print . head $ solve2 numbers
  

solve :: [Int] -> [Int]
solve xs = [ x * y | x <- xs, y <- xs, x+y == 2020]


solve2 :: [Int] -> [Int]
solve2 xs = [ x * y * z | x <- xs, y <- xs, z <- xs, x+y+z == 2020]


testData :: [Int]
testData = [1721, 979, 366, 299, 675, 1456]



