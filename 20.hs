import Data.List
import Data.List.Split
import Data.Char
import Data.Bifunctor

import Control.Monad.State.Lazy

import Data.IntMap (IntMap)
import qualified Data.IntMap as Map


main :: IO ()
main = do
  input <- lines <$> readFile "input20"
  print $ solve input
  print $ solve2 input


type Tile = [[Char]]


-- Each tile can be rotated and flipped in eight different ways:
-- id, three rotations, horizontal flip, vertical flip, diagonal and off-diagonal flip

rotate90, rotate180, rotate270, fliph, flipv, flipd, flipo :: Tile -> Tile
rotate90  = map reverse . transpose
rotate180 = rotate90 . rotate90
rotate270 = rotate90 . rotate90 . rotate90
fliph     = reverse
flipv     = rotate180 . fliph 
flipd     = transpose
flipo     = rotate270 . fliph 


transformations :: [Tile -> Tile]
transformations =
  [id, rotate90, rotate180, rotate270, fliph, flipv, flipd, flipo]


transformTile :: (Int, Tile) -> [(Int, Tile)]
transformTile (a, b) = [ (a, f b) | f <- transformations]


data Direction = North | East | South | West


edge :: Tile -> Direction -> [Char]
edge tile dir =
  case dir of
    North -> head tile
    East  -> map last tile
    South -> last tile
    West  -> map head tile


matching :: Direction -> Tile -> Tile -> Bool
matching direction t1 t2 = 
  case direction of
      East  -> (t1 `edge` East)  == (t2 `edge` West)
      West  -> (t1 `edge` West)  == (t2 `edge` East)
      North -> (t1 `edge` North) == (t2 `edge` South)
      South -> (t1 `edge` South) == (t2 `edge` North)



-- There are 114 = 12*12 tiles, so each row and column of the puzzle contains
-- 12 pieces.

grow :: [(Int, Tile)] -> State (IntMap Tile) [(Int, Tile)]
grow tileRow
  | length tileRow == 12 = return tileRow -- Caution, hard-coded dimension!
  | otherwise = do
      puzzleBox <- get
      
      let (tileKey, tile) = head tileRow
          candidates = concatMap transformTile . Map.toList $ puzzleBox
          matches = filter (matching West tile . snd) candidates
          
      case matches of
        (foundKey, foundTile) : _ -> do modify $ Map.delete foundKey
                                        grow   $ (foundKey, foundTile) : tileRow

        []    ->  fmap reverseRow . grow . reverseRow $ tileRow
                  -- reverseRow <$> grow (reverseRow tileRow)
                  
  where
    reverseRow = reverse . map (bimap id flipv)
    -- reverseRow xs = reverse [ (key, flipv tile) | (key, tile) <- xs ]


growVertical :: [(Int, Tile)] -> State (IntMap Tile) [[ (Int, Tile) ]]
growVertical start = do
  firstRow <- grow start 
  mapM ( grow . singleton . bimap id rotate90 ) firstRow
  where
    singleton x = [x]


solvePuzzle :: [(Int, Tile)] -> [[ (Int, Tile) ]]
solvePuzzle (startPiece : rest) =
  let puzzleBox = Map.fromList rest :: IntMap Tile
  in evalState ( growVertical [startPiece] ) puzzleBox
    

solve :: [String] -> Int
solve input = product . cornerIDs . solvePuzzle $ tiles
  where
    tiles = map unpackTile . filter (not . null) . splitOn [""] $ input

    cornerIDs = fst . unzip . corners

    corners :: [[a]] -> [a]
    corners xs =
      [ head . last $ xs
      , head . head $ xs
      , last . head $ xs
      , last . last $ xs]


unpackTile :: [String] -> (Int, Tile)
unpackTile xs = (n, tile)
  where
    ([header], tile) = splitAt 1 xs
    n = read . takeWhile isDigit . dropWhile (not . isDigit) $ header




-- part2

solve2 input =
  (count '#' $ concat finalGrid) - noOfMonsters * (count '#' $ concat monsterMask)

  where
    count x   = length . filter (== x)
    tiles     = map unpackTile . filter (not . null) . splitOn [""] $ input
    finalGrid = stitchTogether $ map (snd . unzip) $ solvePuzzle tiles
    [noOfMonsters] = filter (/= 0) $ map countMonsters $ transformations <*> [finalGrid]
   

stitchTogether :: [[Tile]] -> Tile
stitchTogether = concat . map mergeRow . map (map removeEdges) 
  where
    removeEdges :: Tile -> Tile
    removeEdges = map init . map tail . init . tail

    mergeRow :: [Tile] -> Tile
    mergeRow = foldl1' (zipWith (++))
    

countMonsters :: Tile -> Int
countMonsters tile = count $ zipWith overlay (repeat monsterMask) (subtiles 20 3 tile)
  where
    count = length . filter id


overlay :: Tile -> Tile -> Bool
overlay mask tile = and $ zipWith f (concat mask) (concat tile)
  where f ' ' _   = True
        f '#' '#' = True
        f _ _     = False


monsterMask :: [String]
monsterMask =
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]


subtiles :: Int -> Int -> Tile -> [Tile]
subtiles x y tile = concatMap (divyRow x) rows 
  where

    rows = divy y tile
    
    divy :: Int -> [a] -> [[a]]
    divy n xs = take (length xs - n + 1) . map (take n) $ tails xs
    -- Divides up an input list into a set of sublists. Each sublist will have n  tems,
    -- and the start of each sublist will be offset by one from the previous one.
    -- divy 3 [1..5] = [[1,2,3], [2,3,4], [3,4,5]]
   
    divyRow :: Int -> [[a]] -> [[[a]]]
    divyRow n xs = transpose . map (divy n) $ xs



testPuzzleBox :: IntMap Tile
testPuzzleBox = Map.fromList . map unpackTile . splitOn [""] $ testInput


testInput :: [String]
testInput =
  [ "Tile 2311:"
  , "..##.#..#."
  , "##..#....."
  , "#...##..#."
  , "####.#...#"
  , "##.##.###."
  , "##...#.###"
  , ".#.#.#..##"
  , "..#....#.."
  , "###...#.#."
  , "..###..###"
  , ""
  , "Tile 1951:"
  , "#.##...##."
  , "#.####...#"
  , ".....#..##"
  , "#...######"
  , ".##.#....#"
  , ".###.#####"
  , "###.##.##."
  , ".###....#."
  , "..#.#..#.#"
  , "#...##.#.."
  , ""
  , "Tile 1171:"
  , "####...##."
  , "#..##.#..#"
  , "##.#..#.#."
  , ".###.####."
  , "..###.####"
  , ".##....##."
  , ".#...####."
  , "#.##.####."
  , "####..#..."
  , ".....##..."
  , ""
  , "Tile 1427:"
  , "###.##.#.."
  , ".#..#.##.."
  , ".#.##.#..#"
  , "#.#.#.##.#"
  , "....#...##"
  , "...##..##."
  , "...#.#####"
  , ".#.####.#."
  , "..#..###.#"
  , "..##.#..#."
  , ""
  , "Tile 1489:"
  , "##.#.#...."
  , "..##...#.."
  , ".##..##..."
  , "..#...#..."
  , "#####...#."
  , "#..#.#.#.#"
  , "...#.#.#.."
  , "##.#...##."
  , "..##.##.##"
  , "###.##.#.."
  , ""
  , "Tile 2473:"
  , "#....####."
  , "#..#.##..."
  , "#.##..#..."
  , "######.#.#"
  , ".#...#.#.#"
  , ".#########"
  , ".###.#..#."
  , "########.#"
  , "##...##.#."
  , "..###.#.#."
  , ""
  , "Tile 2971:"
  , "..#.#....#"
  , "#...###..."
  , "#.#.###..."
  , "##.##..#.."
  , ".#####..##"
  , ".#..####.#"
  , "#..#.#..#."
  , "..####.###"
  , "..#.#.###."
  , "...#.#.#.#"
  , ""
  , "Tile 2729:"
  , "...#.#.#.#"
  , "####.#...."
  , "..#.#....."
  , "....#..#.#"
  , ".##..##.#."
  , ".#.####..."
  , "####.#.#.."
  , "##.####..."
  , "##..#.##.."
  , "#.##...##."
  , ""
  , "Tile 3079:"
  , "#.#.#####."
  , ".#..######"
  , "..#......."
  , "######...."
  , "####.#..#."
  , ".#...#.##."
  , "#.#####.##"
  , "..#.###..."
  , "..#......."
  , "..#.###..."
  ]
