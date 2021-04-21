
import Data.List.Split
import Data.List

main :: IO ()
main = do
  [timeInput, idInput] <- lines <$> readFile "input13"
  
  let startTime = read timeInput :: Int
      ids       = parseIDs  idInput
      ids2      = parseIDs2 idInput

  print $ solve  startTime ids
  print $ solve2 ids2


parseIDs :: String -> [Int]
parseIDs =  map read . filter (/="x") . splitOn ","


solve :: Int -> [Int] -> Int
solve startTime ids = waitingTime * theID
  where 
    -- 'f id' is the earliest time after 'startTime'
    -- the bus with id 'id' is arriving
    f :: Int -> Int
    f id = (startTime `div` id  + 1) * id
    
    (theTime, theID) = minimum $ zip (map f ids) ids

    waitingTime = theTime - startTime


-- part 2

type Offset = Int
type ID = Int

parseIDs2 :: String -> [(Offset, ID)]
parseIDs2 = map (\(x,y) -> (x, read y)) . filter ( (/="x") . snd ) .
            zip [0..] . splitOn ","


solve2 :: [(Offset, ID)] -> Int
solve2 = findTime


findTime :: [(Offset, ID)] -> Int
findTime ids = go ids 1 0
  where
    go :: [(Offset, ID)] -> Int -> Int -> Int
    go []     step time = time
    go (x:xs) step time =
      let (offset, id) = x
      in if (time + offset) `rem` id /= 0    
         then go (x:xs) (step     ) (time + step)
         else go (  xs) (step * id) (time       )



testTime :: Int
testTime = 939

testIDs :: [Int]
testIDs = [7, 13, 59, 31, 19]

testIDs2 :: [(Int,Int)]
testIDs2 = [(0,7), (1,13), (4,59), (6,31), (7,19)]
