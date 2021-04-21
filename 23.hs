
import Data.List

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

import Data.STRef
import Control.Monad
import Control.Monad.ST


main :: IO ()
main = do
  input <- return "962713854"
  print $ solve  input
  print $ solve2 input


parse :: String -> [Int]
parse = map (read . singleton)

singleton = \x -> [x]


move :: [Int] -> [Int]
move (cup : cups) = before ++ (destinationCup : pickUp) ++ after ++ [cup]
  where
    destinationCup = try $ wrap (cup - 1)
    pickUp = take 3 cups
    rest   = drop 3 cups

    destinationIx =
      case elemIndex destinationCup rest of
        Just ix -> ix
        Nothing -> error "This game is rigged!"
        
    (before, _ : after) = splitAt destinationIx rest

    try, wrap :: Int -> Int
    try n = if n `notElem` pickUp then n else try $ wrap (n-1)
    wrap n = (+1) $ (n-1) `mod` 9



solve :: String -> Int
solve = rearrange . play . parse
  where
    play = head . drop 100 . iterate' move
    rearrange = toNum . tail . oneToFront
    toNum = foldl1' (\x y -> x*10 + y)
    oneToFront xs = let (a,b) = span (/=1) xs in b ++ a


testInput = "389125467"




-- part 2

type MemoryList = IntMap [Int]

{- Think of a 'MemoryList' as two stacks (lists), the 'front' and the 'end',
   and an IntMap. The IntMap is used to model inserting elements 'in the middle'
   of the MemoryList.  When the IntMap has the value 'xs' at the key 'x' this is
   understood as 'insert xs after the element x in the 'front' stack'.
   
   Whenever a number is taken from the 'front' stack, we check if
   it is a valid key in the IntMap. If yes the values for this key a are put on
   the stack.

   We 'append' elements to the MemoryList by putting them on the 'end' stack.
   Whenever the 'front' stack is empty we revert the 'end' stack and make a new
   'front' stack out of it.

   Since all numbers stored are positive, we store the 'front' and 'end' stack in
   in the IntMap as well. At position -1 and -2.
-}

mlistNew :: [Int] -> MemoryList
mlistNew  = Map.singleton mlistFront

mlistFront, mlistEnd :: Int
mlistFront = -1
mlistEnd   = -2


solve2 input =  play2 (10^6) (10^7) (parse input)


play2 :: Int -> Int -> [Int] -> Int
play2 size iter firstNineCups = runST $ do
  ref <- newSTRef . mlistNew $ firstNineCups ++ [10..size]
  forM_ [1..iter] $ \_ -> do

      cup    <- getFirst ref
      pickUp <- getNextThree ref
      
      let try, wrap :: Int -> Int
          try  n = if n `notElem` pickUp then n else try $ wrap (n-1)
          wrap n = (+1) $ (n-1) `mod` size
          destinationCup = try $ wrap (cup - 1)
      
      modifySTRef' ref $ Map.insertWith (++) destinationCup pickUp
      modifySTRef' ref $ Map.insertWith (++) mlistEnd [cup]

  multiply . twoAfterOne <$> readSTRef ref
  where multiply = uncurry (*)  



getFirst :: STRef s MemoryList -> ST s Int
getFirst ref = do 
  mlist <- readSTRef ref
  let Just cups = Map.lookup mlistFront mlist
      endCups   = Map.lookup mlistEnd   mlist
      
  case cups of    
    (cup : _) | Nothing <- Map.lookup cup mlist  -> do
                modifySTRef' ref $ Map.adjust (drop 1) mlistFront
                return cup

    (cup : _) | Just pickUp <- Map.lookup cup mlist  -> do
                modifySTRef' ref $ Map.adjust (drop 1)    mlistFront
                modifySTRef' ref $ Map.adjust (pickUp ++) mlistFront
                modifySTRef' ref $ Map.delete cup
                return cup
                                                
    [] | Just cs <- endCups  -> do
           modifySTRef' ref $ Map.insert mlistFront (reverse cs)
           modifySTRef' ref $ Map.delete mlistEnd
           getFirst ref

    [] | Nothing <- endCups  -> error "Empty wheel"


getNextThree :: STRef s (IntMap [Int]) -> ST s [Int]
getNextThree = replicateM 3 . getFirst


twoAfterOne :: MemoryList -> (Int, Int)
twoAfterOne mlist
  | Just (x:_) <- Map.lookup 1 mlist, Just (y:_) <- Map.lookup x mlist
  = (x,y)
  
  | Just (x:y:_) <- Map.lookup 1 mlist, Nothing <- Map.lookup x mlist
  = (x,y)
  
  | otherwise = error "Sorry, case not implemented"
  -- For the particular puzzle input this case is not needed, for a general
  -- solution however it would be. 








