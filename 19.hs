
import Data.Either
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map

import Text.Parsec
import Text.Parsec.String


main :: IO ()
main = do
  input <- lines <$> readFile "input19"
  print $ solve input
  print $ solve2 input


data Rule = Simple [Int]     -- 0: 4 1 5
          | Comb [Int] [Int] -- 1: 2 3 | 3 2
          | Final String     -- 4: "a"
          deriving Show


num :: Parser Int
num = do
  n <- try $ spaces >> many1 digit
  return (read n :: Int)


ruleSimple :: Parser Rule
ruleSimple = do
  ns <- many1 num
  return $ Simple ns


ruleComb :: Parser Rule
ruleComb = do
  ns <- many1 num 
  space >> char '|'
  ms <- many1 num
  return $ Comb ns ms


ruleFinal :: Parser Rule
ruleFinal = do
  space >> char '\"'
  c <- oneOf "ab"
  char '\"'
  return $ Final [c]


rule :: Parser (Int, Rule)
rule = do
  n <- num
  char ':'
  r <- try ruleComb
   <|> try ruleSimple
   <|> try ruleFinal
  spaces >> eof
  return (n, r)



parseRules :: [String] -> IntMap Rule
parseRules = Map.fromList . rights . map (parse rule "")


buildValidator :: IntMap Rule -> Parser String
buildValidator ruleBook = go 0
  where
    go :: Int -> Parser String
    go n =
      case ruleBook Map.! n of
        Final c    -> string c 
        Simple ns  -> combine . recurse $ ns
        Comb ns ms -> try $ choice [ combine . recurse $ ns
                                   , combine . recurse $ ms
                                   ]

    recurse :: [Int] -> [Parser String]
    recurse  = map go
    
    combine :: [Parser String] -> Parser String
    combine  = try . fmap concat . sequence


finalize :: Parser String -> Parser String
finalize p = do
  str <- p
  eof
  return str


{-
-- A function that, instead of parsing the strings defined by the grammar,
-- produces all of them. Note the similarity to buildValidator.

produce :: IntMap Rule -> [String]
produce ruleBook n = go 0
  where
    go n = 
      case ruleBook Map.! n of
        Final c    -> return c
        Simple ns  -> map concat . sequence $ map go ns
        Comb ns ms -> (map concat . sequence $ map go ns)
                      ++ (map concat . sequence $ map go ms)
-}


solve :: [String] -> Int
solve input = length . rights . map (parse validator "") $ messages
  where
    messages  = drop 1 . dropWhile isRule $ input  
    rules     = takeWhile isRule input
    isRule    = not . null
                 
    ruleBook :: IntMap Rule
    ruleBook  = parseRules rules

    validator :: Parser String
    validator = finalize $ buildValidator ruleBook



testInput :: [String]
testInput =
  ["0: 4 1 5"
  ,"1: 2 3 | 3 2"
  ,"2: 4 4 | 5 5"
  ,"3: 4 5 | 5 4"
  ,"4: \"a\""
  ,"5: \"b\""
  ,"6: 4 | 4 6"
  ,""
  ,"ababbb"
  ,"bababa"
  ,"abbbab"
  ,"aaabbb"
  ,"aaaabbb"]


testRuleBook :: IntMap Rule
testRuleBook  = parseRules $ takeWhile (not . null) testInput

testValidator :: Parser String
testValidator = finalize $ buildValidator testRuleBook



-- part 2

solve2 :: [String] -> Int
solve2 input = length . rights . map (parse validator "") $ messages
  where
    messages  = drop 1 . dropWhile isRule $ input  
    rules     = takeWhile isRule input
    isRule    = not . null

    ruleBook :: IntMap Rule
    ruleBook  = hackyPatch $ parseRules rules -- here is the difference

    validator :: Parser String
    validator = finalize $ buildValidator ruleBook



hackyPatch :: IntMap Rule -> IntMap Rule
hackyPatch book = update `Map.union` book
  where
    update = Map.fromList 
      [ ( 0, Comb [42,11]                               [-1])
      , (-1, Comb [42,42,11]                            [-2])
      , (-2, Comb [42,42,42,11]                         [-3])
      , (-3, Comb [42,42,42,42,11]                      [-4])
      , (-4, Comb [42,42,42,42,42,11]                   [-5])
      , (-5, Comb [42,42,42,42,42,42,11]                [-6])
      , (-6, Comb [42,42,42,42,42,42,42,11]             [-7])
      , (-7, Comb [42,42,42,42,42,42,42,42,11]          [-8])
      , (-8, Comb [42,42,42,42,42,42,42,42,42,11]
                  [42,42,42,42,42,42,42,42,42,42,11])
        
      
      , (11,   Comb [42,31]                                                 [1101])
      , (1101, Comb [42,42,31,31]                                           [1102])
      , (1102, Comb [42,42,42,31,31,31]                                     [1103])
      , (1103, Comb [42,42,42,42,31,31,31,31]                               [1104])
      , (1104, Comb [42,42,42,42,42,31,31,31,31,31]                         [1105])
      , (1105, Comb [42,42,42,42,42,42,31,31,31,31,31,31]                   [1106]) 
      , (1106, Comb [42,42,42,42,42,42,42,31,31,31,31,31,31,31]             [1107])
      , (1107, Comb [42,42,42,42,42,42,42,42,31,31,31,31,31,31,31,31]       [1108])
      , (1108, Comb [42,42,42,42,42,42,42,42,42,31,31,31,31,31,31,31,31,31]
                    [42,42,42,42,42,42,42,42,42,42,31,31,31,31,31,31,31,31,31,31])
      ]



testRuleBook2 :: IntMap Rule
testRuleBook2 = parseRules $  takeWhile (not . null) testInput2

testInput2 :: [String]
testInput2 = 
  ["42: 9 14 | 10 1"
  ,"9: 14 27 | 1 26"
  ,"10: 23 14 | 28 1"
  ,"1: \"a\""
  ,"11: 42 31"
  ,"5: 1 14 | 15 1"
  ,"19: 14 1 | 14 14"
  ,"12: 24 14 | 19 1"
  ,"16: 15 1 | 14 14"
  ,"31: 14 17 | 1 13"
  ,"6: 14 14 | 1 14"
  ,"2: 1 24 | 14 4"
  ,"0: 8 11"
  ,"13: 14 3 | 1 12"
  ,"15: 1 | 14"
  ,"17: 14 2 | 1 7"
  ,"23: 25 1 | 22 14"
  ,"28: 16 1"
  ,"4: 1 1"
  ,"20: 14 14 | 1 15"
  ,"3: 5 14 | 16 1"
  ,"27: 1 6 | 14 18"
  ,"14: \"b\""
  ,"21: 14 1 | 1 14"
  ,"25: 1 1 | 1 14"
  ,"22: 14 14"
  ,"8: 42"
  ,"26: 14 22 | 1 20"
  ,"18: 15 15"
  ,"7: 14 5 | 1 21"
  ,"24: 14 1"
  ,""
  ,"abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
  ,"bbabbbbaabaabba"
  ,"babbbbaabbbbbabbbbbbaabaaabaaa"
  ,"aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
  ,"bbbbbbbaaaabbbbaaabbabaaa"
  ,"bbbababbbbaaaaaaaabbababaaababaabab"
  ,"ababaaaaaabaaab"
  ,"ababaaaaabbbaba"
  ,"baabbaaaabbaaaababbaababb"
  ,"abbbbabbbbaaaababbbbbbaaaababb"
  ,"aaaaabbaabaaaaababaa"
  ,"aaaabbaaaabbaaa"
  ,"aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
  ,"babaaabbbaaabaababbaabababaaab"
  ,"aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"]


