import Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main = do
  input <- lines <$> readFile "input22"
  print $ solve  input
  print $ solve2 input


type Deck = [Int]

solve = sum . zipWith (*) [1..] . reverse . play . parse



play :: (Deck, Deck) ->  Deck
play ([], es) = es
play (ds, []) = ds
play (d:ds, e:es)
  | d < e  = play (ds, es ++ [e,d]) 
  | d > e  = play (ds ++ [d,e], es) 



parse :: [String] -> (Deck, Deck)
parse xs = (a, b)
  where
    a = map read . drop 1 . takeWhile (not . null) $ xs
    b = map read . drop 2 . dropWhile (not . null) $ xs






-- part 2


type History = Set (Deck, Deck)

inHistory :: (Deck, Deck) -> History -> Bool
inHistory = Set.member

insertIn :: (Deck, Deck) -> History -> History
insertIn = Set.insert




solve2 :: [String] -> Int
solve2 = calcScore . playGame . parse
  where 
   calcScore (xs, []) = sum . zipWith (*) [1..] . reverse $ xs
   calcScore ([], xs) = calcScore (xs, [])



playGame :: (Deck, Deck) -> (Deck, Deck)
playGame ([], bs) = ([], bs)
playGame (as, []) = (as, [])
playGame (as, bs) = playRound (as, bs, Set.empty)


playRound :: (Deck, Deck, History) -> (Deck, Deck)
playRound ([], bs, _) = ([], bs)
playRound (as, [], _) = (as, [])

playRound (deck1, deck2, h)
  | (deck1, deck2) `inHistory` h = (deck1, deck2) 
  -- If the same deck constellation has occured previously in the game,
  -- the game ends with Player 1 as the winner, see comment below.
  
  | let (a:as, b:bs) = (deck1, deck2), length as >= a, length bs >= b
  = case playGame (take a as, take b bs) of -- play subgame
      
      ([], _  ) -> playRound (as, bs ++ [b,a], updatedHistory)
      (_ , [] ) -> playRound (as ++ [a,b], bs, updatedHistory)
         
      otherwise  -> playRound (as ++ [a,b], bs, updatedHistory)
                    -- here Player 1 is interpreted as the winner

  | otherwise, let (a:as, b:bs) = (deck1, deck2) 
  = case a `compare` b of

      LT -> playRound (as, bs ++ [b,a], updatedHistory)
      GT -> playRound (as ++ [a,b], bs, updatedHistory)
      _  -> error "Each card should be in the deck just once"
 
  where
    updatedHistory = (deck1, deck2) `insertIn` h




-- possible improvement: use Seq instead of a list for the decks


testInput = 
  ["Player 1:"
  ,"9"
  ,"2"
  ,"6"
  ,"3"
  ,"1"
  ,""
  ,"Player 2:"
  ,"5"
  ,"8"
  ,"4"
  ,"7"
  ,"10"]
