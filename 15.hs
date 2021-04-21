

import Data.List
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as IntMap

--part 2
import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as V




main :: IO ()
main = do
  let input = [0,13,16,17,1,10,6]
  print $ solve   input 2020
  print $ solveST input (3*10^7)


type GameState = ( Int          -- current round
                 , Int          -- last number spoken
                 , IntMap Int   -- key: number
                 )              -- value: last turn when key number was spoken, not
                                -- including the previous round


play :: GameState -> GameState
play (turn, lastNum, memory) =
  let thisNum = case memory !? lastNum of  
                  Nothing -> 0 
                  Just x  -> lastTurn - x
  in (turn + 1, thisNum, remember lastNum)
  where
    lastTurn   = turn - 1
    remember n = IntMap.insert n lastTurn memory


solve :: [Int] -> Int -> Int 
solve input n = getResult $ iterate' play initialState
  where   
    initialState =
      ( length input + 1
      , last input
      , IntMap.fromList $ zip input [1..]
      )      
    getResult xs = snd $ xs !! (n - (length input))
    snd (_, x, _) = x


testInitialState :: GameState
testInitialState = (4, 6, IntMap.fromList [(0, 1), (3,2), (6,3)] )


-- part 2

playST :: STVector s Int -> Int -> Int -> (ST s) Int
playST vec lastNum turn = do
  turnLastNumSpokenBefore <- vec `V.read` lastNum
  let thisNum = case turnLastNumSpokenBefore of 
                  0 -> 0
                  x -> lastTurn - x
  V.write vec lastNum lastTurn
  return thisNum
  where
    lastTurn = turn -1 


solveST :: [Int] -> Int -> Int
solveST input n = runST $ do
  vec <- V.replicate initLen 0 
  zipWithM_ (V.write vec) (init input) [1..]
  
  foldM (playST vec) (last input) [firstTurn .. n]
  where
    initLen   = (+1) $ max n (maximum input)
    firstTurn = length input + 1
