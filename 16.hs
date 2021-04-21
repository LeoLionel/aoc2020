
import Data.List
import Data.List.Split

import Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do
  input <- lines <$> readFile "input16"
  print $ solve input
  print $ solve2 input



type Rule = Int -> Bool
type FieldName = String
type Ticket = [Int]


parseRule :: String -> (FieldName, Rule)
parseRule str = (name,  \x ->  x `inRange` r1 || x `inRange` r2)
  where
    (name, rest) = break (== ':') str
    [r1, r2] = map readRange . splitOn " or " . drop 2 $ rest

    -- readRange "42-77" == (42,77)
    readRange :: String -> (Int, Int)
    readRange str =
      let [a,b] = splitOn "-" str
      in (read a, read b)

    -- 7 `inRange` (3,10) == True
    inRange :: Int -> (Int, Int) -> Bool
    inRange n (low, high) = low <= n && n <= high



parseRules :: [String] -> Map FieldName Rule
parseRules input = Map.fromList . fmap parseRule $ take 20 input

parseMyTicket :: [String] -> Ticket
parseMyTicket input = map read . splitOn "," . head . drop 22 $ input

parseNearbyTickets :: [String] -> [Ticket]
parseNearbyTickets input = map (map read . splitOn ",") . drop 25 $ input



validForSomeRule :: Map FieldName Rule -> Int -> Bool
validForSomeRule rules n = any (\rule -> rule n) rules

invalidFields :: Map FieldName Rule  -> Ticket -> [Int]
invalidFields rules ticket = filter (not . validForSomeRule rules) ticket


solve :: [String] -> Int
solve input = sum . concatMap (invalidFields rules) $ tickets
  where 
    rules   = parseRules input          :: Map FieldName Rule
    tickets = parseNearbyTickets input  :: [Ticket]



-- part 2


solve2 :: [String] -> Int
solve2 input =
  product [ myTicket !! i | i <- departurePositions ]
  where
    rules    = parseRules input          :: Map FieldName Rule
    tickets  = parseNearbyTickets input  :: [Ticket]
    myTicket = parseMyTicket input       :: Ticket
    
    validTickets = filter (`validBy` rules) tickets :: [Ticket]

    positions = findPositions validTickets rules :: Map FieldName Int
    departurePositions =
     Map.elems . Map.filterWithKey (\key _ -> "departure" `isPrefixOf` key) $ positions
    


validBy :: Ticket -> Map FieldName Rule -> Bool
validBy ticket rules = all (validForSomeRule rules) ticket


findPositions :: [Ticket] -> Map FieldName Rule -> Map FieldName Int
findPositions tickets rules = eliminate $ possiblePositions tickets rules


possiblePositions :: [Ticket] -> Map FieldName Rule -> Map FieldName [Int]
possiblePositions tickets rules =
  fmap f rules 
    where   
    f :: Rule -> [Int]
    f rule = findIndices (\xs -> all rule xs) (transpose tickets)


eliminate :: Map FieldName [Int] -> Map FieldName Int
eliminate dict
  | all isSingleton dict = fmap head dict
  | otherwise =
    let found = concat . Map.elems . Map.filter isSingleton $ dict :: [Int]
        dict' = fmap f dict
        f [x] = [x]
        f xs  = filter (`notElem` found) xs
    in eliminate dict'


isSingleton :: [a] -> Bool
isSingleton [x] = True 
isSingleton _   = False






testRules :: Map FieldName Rule
testRules = parseRules $
  ["class: 1-3 or 5-7"
  ,"row: 6-11 or 33-44"
  ,"seat: 13-40 or 45-50"]

testTickets :: [Ticket]
testTickets = map (map read . splitOn ",") $
  ["7,3,47"
  ,"40,4,50"
  ,"55,2,20"
  ,"38,6,12"]


testRules2 :: Map FieldName Rule
testRules2 = parseRules $
  ["class: 0-1 or 4-19"
  ,"row: 0-5 or 8-19"
  ,"seat: 0-13 or 16-19"]
  
testTickets2 :: [Ticket]
testTickets2 = map (map read . splitOn ",") $
  ["3,9,18"
  ,"15,1,5"
  ,"5,14,9"]
