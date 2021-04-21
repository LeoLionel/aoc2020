
import Data.Array

main :: IO ()
main = do
  input <- lines <$> readFile "input11"
  print $ solve  input
  print $ solve2 input


type Position = (Int, Int)
type Grid = Array (Int, Int) Char
type Rule = Char -> [Char] -> Char


makeGrid :: [[Char]] -> Grid
makeGrid xss =
  array bounds [ (i, pad xss `take` i) | i <- range bounds ]
  where
    bounds = ((-1,-1), (l,l))
    l = length xss
    take yss i = yss !! (snd i + 1) !! (fst i + 1)


pad :: [[Char]] -> [[Char]]
pad xss = [columnPad] ++ map padRow xss ++ [columnPad]
  where
    columnPad = replicate (length xss + 2) '.'
    padRow xs = ['.'] ++ xs ++ ['.']


getNeighbours :: Position -> Grid -> [Char]
getNeighbours pos g =
  [ g ! i | i <- ixs , i /= pos ]
  where
    (x,y) = pos
    ixs   = range ((x-1,y-1), (x+1,y+1))


updateRule :: Rule
updateRule seat neighbours =
  case seat of
    'L' | (==0) . count '#' $ neighbours  -> '#' 
    '#' | (>3)  . count '#' $ neighbours  -> 'L'
    otherwise                             -> seat
   where
     count x = length . filter (==x)


updateGrid :: Rule -> Grid -> Grid 
updateGrid updateRule grid =
  grid // [ (i, updatePos i) | i <- interior ]
  
  where
    updatePos j = updateRule (grid ! j) (getNeighbours j grid)
    interior = range ((0,0), (high-1, high-1))
    ((_,_),(_,high)) = bounds grid


evolve :: Eq a =>  (a -> a) -> a -> a
evolve f x = go f x (f x)
  where
    go f x y =
      if x == y
      then x
      else go f y (f y)


solve :: [String] -> Int 
solve input = count '#' . elems $ evolve update grid 
  where
    update  = updateGrid updateRule
    grid    = makeGrid input
    count x = length . filter (==x)


-- part 2


diagonals :: Position -> Grid -> [[Char]]
diagonals (x,y) g =
  let ((_,_),(_,high)) = bounds g
      l = high - 1
  in [ [ g ! (x, y - i)     | i <- [1..y] ]             -- North 
     , [ g ! (x + i, y )    | i <- [1..(l-x)] ]         -- East
     , [ g ! (x, y + i )    | i <- [1..(l-y)] ]         -- South
     , [ g ! (x - i, y)     | i <- [1..x] ]             -- West

     , [ g ! (x + i, y - i) | i <- [1..(min y (l-x))] ]     -- North East
     , [ g ! (x + i, y + i) | i <- [1..(min (l-x) (l-y))] ] -- South East
     , [ g ! (x - i, y + i) | i <- [1..(min x (l-y))] ]     -- South West 
     , [ g ! (x - i, y - i) | i <- [1..(min x y)] ]         -- North West
     ]



getVisible :: Position -> Grid -> [Char]
getVisible i g =
  let d = map (dropWhile (=='.')) $ diagonals i g
      safeHead (x:xs) = x
      safeHead []     = '.' 
  in map safeHead d


updateRule2 :: Rule
updateRule2 seat visible =
  case seat of
    'L' | (==0) . count '#' $ visible  -> '#' 
    '#' | (>4)  . count '#' $ visible  -> 'L'
    otherwise                          -> seat
  where
    count x = length . filter (==x)


solve2 :: [String] -> Int 
solve2 input = count '#' . elems $ evolve update grid 
  where
    update  = updateGrid updateRule2 
    grid    = makeGrid input
    count x = length . filter (==x)




testInput :: [[Char]]
testInput =
  ["L.LL.LL.LL"
  ,"LLLLLLL.LL"
  ,"L.L.L..L.."
  ,"LLLL.LL.LL"
  ,"L.LL.LL.LL"
  ,"L.LLLLL.LL"
  ,"..L.L....."
  ,"LLLLLLLLLL"
  ,"L.LLLLLL.L"
  ,"L.LLLLL.LL"]


testInput2 :: [[Char]]
testInput2 =
  [".......#."
  ,"...#....."
  ,".#......."
  ,"........."
  ,"..#L....#"
  ,"....#...."
  ,"........."
  ,"#........"
  ,"...#....."]
