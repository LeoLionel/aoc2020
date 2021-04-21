
import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.State


main :: IO ()
main = do
  input <- lines <$> readFile "input8"
  print $ solve  input
  print $ solve2 input


type Counter = Int
type Accum = Int

type Operation = (Counter, Accum) -> (Counter, Accum)

type Tape  = Map Int Operation
type Trace = Set Int


parseOperation :: String -> Operation
parseOperation str =
  let op = take 3 str
      n  = drop 5 str
      plusOrMinus = take 1 . drop 4 $ str
      sign = if plusOrMinus == "+" then id else negate
      num  = sign $ (read n) :: Int
  in case op of
    "acc"  ->  \(x, y) -> (x + 1, y + num)
    "jmp"  ->  \(x, y) -> (x + num, y)
    "nop"  ->  \(x, y) -> (x + 1, y)


machine :: (Counter, Accum) -> State (Tape, Trace) (Counter, Accum)
machine (counter, accum) = do
  tapeLength <- gets (length . fst)
  trace      <- gets snd
  
  case counter of
    
    _ | counter >= tapeLength -> return (101, accum)
                              -- set counter to 101 to signal normal exit
                                       
    _ | counter `Set.member` trace -> return (404, accum)
                                   -- set counter to 101 to signal infinite loop
                                      
    otherwise -> do nextOperation <- gets $ (Map.! counter) . fst
                    modify $ \(x,y) -> (x, counter `Set.insert` y)
                    machine $ nextOperation (counter, accum) 


runMachine :: Tape -> (Counter, Accum)
runMachine tape = evalState (machine initialValue) (tape, emptyTrace) 
  where
    initialValue = (0, 0)  :: (Counter, Accum)
    emptyTrace = Set.empty :: Trace

    
solve :: [String] -> Accum
solve input = snd $ runMachine tape
  where
    tape  = Map.fromList $ zip [0..] operations 
    operations = map parseOperation input
    


-- part 2 


flipJmpAndNop :: String -> String
flipJmpAndNop str =
  case take 3 str of 
    "jmp"     -> "nop" ++ drop 3 str
    "nop"     -> "jmp" ++ drop 3 str
    otherwise -> str


programCandidates :: [String] -> [Tape]
programCandidates input = 
  let
    sourceCode :: Map Int String
    sourceCode = Map.fromList $ zip [0..] input
    
    indices :: [Int]
    indices = findIndices (\x -> "jmp" `isPrefixOf` x || "nop" `isPrefixOf` x) input

    sourceCodeCandidates :: [Map Int String]
    sourceCodeCandidates = [ Map.adjust flipJmpAndNop i sourceCode| i <- indices ]

  in [ fmap parseOperation cand | cand <- sourceCodeCandidates ] :: [Tape]
    

solve2 :: [String] -> Accum
solve2 input = snd . head $ filter successfulExit candidateResults
  where
    candidateResults :: [(Counter, Accum)]
    candidateResults = map runMachine $ programCandidates input

    successfulExit :: (Counter, Accum) -> Bool
    successfulExit (101, _) = True
    successfulExit _        = False


testData :: [String]
testData =
  ["nop +0"
  ,"acc +1"
  ,"jmp +4"
  ,"acc +3"
  ,"jmp -3"
  ,"acc -99"
  ,"acc +1"
  ,"jmp -4"
  ,"acc +6"]


testData2 :: [String]
testData2 =
  ["nop +0"
  ,"acc +1"
  ,"jmp +4"
  ,"acc +3"
  ,"jmp -3"
  ,"acc -99"
  ,"acc +1"
  ,"nop -4"
  ,"acc +6"]
