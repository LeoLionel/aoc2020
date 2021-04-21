
import Text.Parsec hiding (Line)
import Text.Parsec.String

import Data.List   (sort, group)
import Data.Either (lefts, rights)


main :: IO ()
main = do
  input <- lines <$> readFile "input2"

  let (parseReport, result1) = solve valid  input
      (_          , result2) = solve valid2 input

  putStrLn parseReport
  print result1
  print result2

      
type ParserReport = String
type Line = (Char, Int, Int, String)


solve :: (Line -> Bool) -> [String] -> (ParserReport, Int) 
solve policy input =
  let lines = map (parse parserLine "") input
      parseReport = case lefts lines of
        [] -> "Parse OK"
        xs -> "Warning! Could not parse " ++ (show $ length xs) ++ " lines"
        
      num = length . filter policy $ rights lines
      
  in (parseReport, num)

  

nat :: Parser Int
nat = (\x -> read x :: Int) <$> many1 digit

parserLine :: Parser Line
parserLine  = do
  x <- nat
  char '-'
  y <- nat
  space
  c <- letter
  char ':'
  space
  password <- many1 letter
  spaces
  return (c, x, y, password)


valid :: Line -> Bool
valid (c, low, high, password) =
  let maybeFreq = lookup c $ charFrequencyList password
  in case maybeFreq of
       Just freq -> freq `inRange` (low, high)
       Nothing   -> False


-- part 2

charFrequencyList :: String -> [(Char,Int)]
charFrequencyList = map (\xs -> (head xs, length xs)) . group . sort


inRange :: Int -> (Int, Int) -> Bool
inRange x (low, high) = low <= x && x <= high 


valid2 :: Line -> Bool
valid2 (c, pos1, pos2, pw) =
  let mx = lookup pos1 $ zip [1..] pw
      my = lookup pos2 $ zip [1..] pw
      mc = Just c
  in (mc == mx) `xor` (mc == my)


xor :: Bool -> Bool -> Bool
xor a b = ( a || b ) && not ( a && b )


testInput :: [String]
testInput =
  ["1-3 a: abcde"
  ,"1-3 b: cdefg"
  ,"2-9 c: ccccccccc"]
