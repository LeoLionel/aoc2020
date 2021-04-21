

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main = do
  input <- lines <$> readFile "input17"
  print $ solve  (readGrid  input)
  print $ solve2 (readGrid2 input)
  

type Grid = Set Pos
type Pos = (Int, Int, Int)


readGrid :: [[Char]] -> Grid
readGrid input = 
 Set.fromList [ (x, y, 0) | y <- [0..l]
                          , x <- [0..l]
                          , input !! y !! x == '#']
  where l = length input - 1 


section :: Pos -> Grid -> Grid
section (x,y,z) g =
  g `Set.intersection` n
  where
    n = Set.fromList [ (x+i, y+j, z+k) | i <- [-1,0,1]
                                       , j <- [-1,0,1]
                                       , k <- [-1,0,1]
                                       ]


evolve :: Grid -> Grid
evolve grid = Set.fromList 
  [ (i,j,k) | i <- xrange , j <- yrange, k <- zrange
            ,    aliveNeighbours (i,j,k) == 3
              || aliveNeighbours (i,j,k) == 4 && alive (i,j,k)
            ]
  where
    alive pos = pos `Set.member` grid 
    aliveNeighbours pos = Set.size (pos `section` grid)
    
    xrange = [(minimum xs - 1) .. (maximum xs + 1)]
    yrange = [(minimum ys - 1) .. (maximum ys + 1)]
    zrange = [(minimum zs - 1) .. (maximum zs + 1)]
    (xs, ys, zs) = unzip3 . Set.toList $ grid
    

solve :: Grid -> Int
solve grid = Set.size . head . drop 5 $ iterate evolve grid



testInput :: [[Char]]
testInput = 
  [".#."
  ,"..#"
  ,"###"]

testGrid :: Grid
testGrid =
  Set.fromList [ (x-1 , y-1, 0) | y <- [0..2]
                                , x <- [0..2]
                                , testInput !! y !! x == '#']



-- part 2

type Grid2 = Set Pos2
type Pos2 = (Int, Int, Int, Int)


readGrid2 :: [[Char]] -> Grid2
readGrid2 input = 
 Set.fromList [ (x, y, 0, 0) | y <- [0..l]
                             , x <- [0..l]
                             , input !! y !! x == '#']
  where l = length input - 1 


section2 :: Pos2 -> Grid2 -> Grid2
section2 (x,y,z,w) g =
  g `Set.intersection` n
  where n = Set.fromList [ (x+i, y+j, z+k, w+l) | i <- [-1,0,1]
                                                , j <- [-1,0,1]
                                                , k <- [-1,0,1]
                                                , l <- [-1,0,1]]


evolve2 :: Grid2 -> Grid2
evolve2 grid = Set.fromList 
  [ (i,j,k,l) | i <- xrange , j <- yrange, k <- zrange, l <- wrange
              ,    aliveNeighbours (i,j,k,l) == 3
                || aliveNeighbours (i,j,k,l) == 4 && alive (i,j,k,l)
              ]
  where
    alive pos = pos `Set.member` grid
    aliveNeighbours pos  = Set.size (pos `section2` grid)
    
    xrange = [(minimum xs - 1) .. (maximum xs + 1)]
    yrange = [(minimum ys - 1) .. (maximum ys + 1)]
    zrange = [(minimum zs - 1) .. (maximum zs + 1)]
    wrange = [(minimum ws - 1) .. (maximum ws + 1)]
    (xs, ys, zs, ws) = unzip4 . Set.toList $ grid


solve2 :: Grid2 -> Int
solve2 grid = Set.size . head . drop 5 $ iterate evolve2 grid









