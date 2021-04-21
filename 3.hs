
main :: IO ()
main = do
  input <- lines <$> readFile "input3"

  let result1 = solve (3,1) input
  
      slopes  = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      result2 = product [solve s input | s <- slopes]
      
  print result1
  print result2


type Slope = (Int, Int)
type Grid  = [[Char]]


pathBy :: Slope -> Grid -> [Char]
pathBy (dx, dy) grid =
  let l   = length grid
      n   = (l - 1) `div` dy -- number of full steps from start field
      pos = [ (dx * i, dy * i) | i <- [0,1..n] ] -- coordinates of the path

  in [ grid !! y !! x  | (x,y) <- pos ]


countTrees :: [Char] -> Int
countTrees = length . filter (== '#')


solve :: Slope -> Grid -> Int
solve slope input = countTrees $ pathBy slope (map cycle input)


testData :: Grid
testData =
  ["..##......."
  ,"#...#...#.."
  ,".#....#..#."
  ,"..#.#...#.#"
  ,".#...##..#."
  ,"..#.##....."
  ,".#.#.#....#"
  ,".#........#"
  ,"#.##...#..."
  ,"#...##....#"
  ,".#..#...#.#"
  ]
