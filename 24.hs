
import Text.Parsec
import Text.Parsec.String

import Data.Set (Set)
import qualified Data.Set as Set


import Data.Either
import Data.List


main :: IO ()
main = do
  input <- lines <$> readFile "input24"
  print $ solve input
  print $ solve2 input


type TilePos = (Int, Int)
type Move = TilePos -> TilePos


moveE, moveNE, moveW, moveSW, moveSE, moveNW :: Move
moveE  (n, m) = (n+1, m  )
moveNE (n, m) = (n  , m+1)
moveW  (n, m) = (n-1, m  )
moveSW (n, m) = (n  , m-1)
moveSE (n, m) = (n+1, m-1)
moveNW (n, m) = (n-1, m+1)


parseMove :: Parser Move
parseMove =
      (char 'e' >> return moveE)
  <|> (char 'w' >> return moveW)
  <|> (try $ string "nw" >> return moveNW)
  <|> (try $ string "ne" >> return moveNE)
  <|> (try $ string "sw" >> return moveSW)
  <|> (try $ string "se" >> return moveSE)


parseMoves :: Parser [Move]
parseMoves = do
  ms <- many1 parseMove
  eof
  return ms


apply :: [a -> a] -> a -> a
apply [] x = x
apply (f:fs) x = (f x) `seq` apply fs (f x)


solve input = length $ filter (not . even . length) . group . sort $ finalTiles
  where
    sequences = rights . map (parse parseMoves "") $ input
    finalTiles = map ( \fs -> apply fs (0,0) ) sequences
    


-- part 2




type Grid = Set Pos
type Pos = (Int, Int)


section :: Pos -> Grid -> Grid
section pos g =
  g `Set.intersection` n
  where
    n = Set.fromList $ [moveE, moveNE, moveW, moveSW, moveSE, moveNW] <*> [pos]

evolve :: Grid -> Grid
evolve g = Set.fromList 
  [ (i,j) | i <- xrange , j <- yrange
          , black (i,j) && blackNeighbours (i,j) `elem` [1,2]
          || white (i,j) && blackNeighbours (i,j) == 2 ]
  where
    black p = p `Set.member` g
    white p = not $ black p
    
    blackNeighbours p = Set.size (p `section` g)
    
    xrange = [(minimum xs - 1) .. (maximum xs + 1)]
    yrange = [(minimum ys - 1) .. (maximum ys + 1)]
    (xs, ys) = unzip . Set.toList $ g
    




solve2 input = Set.size . head . drop 100 . iterate' evolve $ startGrid
  where
    sequences = rights . map (parse parseMoves "") $ input
    finalTiles = map ( \fs -> apply fs (0,0) ) sequences
    startGrid = Set.fromList $ concat . filter (not . even . length) .
                group . sort $ finalTiles


ti =
  ["sesenwnenenewseeswwswswwnenewsewsw"
  ,"neeenesenwnwwswnenewnwwsewnenwseswesw"
  ,"seswneswswsenwwnwse"
  ,"nwnwneseeswswnenewneswwnewseswneseene"
  ,"swweswneswnenwsewnwneneseenw"
  ,"eesenwseswswnenwswnwnwsewwnwsene"
  ,"sewnenenenesenwsewnenwwwse"
  ,"wenwwweseeeweswwwnwwe"
  ,"wsweesenenewnwwnwsenewsenwwsesesenwne"
  ,"neeswseenwwswnwswswnw"
  ,"nenwswwsewswnenenewsenwsenwnesesenew"
  ,"enewnwewneswsewnwswenweswnenwsenwsw"
  ,"sweneswneswneneenwnewenewwneswswnese"
  ,"swwesenesewenwneswnwwneseswwne"
  ,"enesenwswwswneneswsenwnewswseenwsese"
  ,"wnwnesenesenenwwnenwsewesewsesesew"
  ,"nenewswnwewswnenesenwnesewesw"
  ,"eneswnwswnwsenenwnwnwwseeswneewsenese"
  ,"neswnwewnwnwseenwseesewsenwsweewe"
  ,"wseweeenwnesenwwwswnew"]
