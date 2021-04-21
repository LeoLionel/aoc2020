
import Data.Either
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec hiding (count)
import Text.Parsec.String


main :: IO ()
main = do
  input <- lines <$> readFile "input7"
  let rules = rights $ map (parse bagRule "") input
      
  print $ solve  rules
  print $ solve2 rules
  

{- lets try to write a grammar for the rules

<bag rule>    = <bag id> 'bags contain' <bag content> '.'
<bag content> = <empty bag> | <many rules>
<empty bag>   = 'no other bags'
<many rules>  = <one rule> ',' <one rule> , ...
<one rule>    = <num> <bag id> 'bag' | <num> <bag id> 'bags'
<bag id>      = <word> <word>
<num>         = '1' | '2' | ... | '9'
-}

type Rule = (String, [(Int, String)])

bagRule :: Parser (String, [(Int, String)])
bagRule = do
  id <- bagID
  string " bags contain "
  r <- bagContent
  char '.'
  return (id, r)

bagContent :: Parser [(Int, String)]
bagContent = choice [emptyBag, manyRules]

emptyBag :: Parser [a]
emptyBag = do
  string "no other bags"
  return []

manyRules :: Parser [(Int, String)]
manyRules = oneRule `sepBy1` (char ',' >> space)

oneRule :: Parser (Int, String)
oneRule = do
  n  <- num
  space
  id <- bagID
  space
  string "bag" >> optional (char 's')
  return (n, id)

bagID :: Parser String
bagID = do
  w1 <- many1 letter
  space
  w2 <- many1 letter
  return (w1 ++ " " ++ w2)

num :: Parser Int
num = toInt <$> oneOf ['1'..'9']
  where
    toInt :: Char -> Int
    toInt x = fromEnum x - fromEnum '0' 


{- 
The data structure we end up with after parsing can be seen as a
(directed) graph. This is more obvious when we rename the
colours ('light red' will be A, 'dark orange' B, ... ) and write
down the example rules (leaving out the numbers). The arrow means
'countain'. 

  light red:      A -> C, D
  dark orange:    B -> C, F
  bright white:   C -> E
  muted yellow:   D -> E, H
  shiny gold:     E -> F, G
  dark olive:     F -> H, J
  vibrant plum:   G -> H, J 
  faded blue:     H -> 
  dotted black:   J -> 

The questions if a bag of colour X contains a bag of color Y, is then translated
to the question if we can reach Y from X.
-}


type Key = String
type Graph = Map Key [Key]


removeNumbers :: [Rule] -> [(Key, [Key])]
removeNumbers = map (\ (a, x) -> (a, snd . unzip $ x) )


path :: Key -> Key -> Graph -> Bool
path k1 k2 graph =
  if k1 == k2
    then False
    else go k1 k2 
  where       
    go currentPos end
      | currentPos == end  = True
      | otherwise =
        case Map.lookup currentPos graph of
          Nothing -> False
          Just positions -> any (\x -> go x end) positions


solve :: [Rule] -> Int
solve rules = length . filter canContainShinyGold $ keys
  where
    canContainShinyGold key = path key "shiny gold" graph
    graph = Map.fromList $ removeNumbers rules
    keys  = Map.keys graph


-- part 2, now we need to work with the numbered graph

type GraphNumbered = Map Key [(Int, Key)]


totalContent :: GraphNumbered -> Key -> Int
totalContent g k =
  case Map.lookup k g of
    Just [] -> 0
    Just xs ->
      let (nums, keys) = unzip xs
          numOfBags    = sum nums
          bagsContent  = sum $ zipWith (*) nums [ totalContent g key | key <- keys]
       in numOfBags + bagsContent


solve2 :: [Rule] -> Int
solve2 rules = totalContent graph "shiny gold"
  where
    graph = Map.fromList rules






testRules :: [Rule]
testRules = rights . map (parse bagRule "") $ testInput

testInput :: [String]
testInput =
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
  ,"dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  ,"bright white bags contain 1 shiny gold bag."
  ,"muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  ,"shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  ,"dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  ,"vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  ,"faded blue bags contain no other bags."
  ,"dotted black bags contain no other bags."]
