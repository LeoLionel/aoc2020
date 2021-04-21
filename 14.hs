
import Control.Monad
import Data.Bits
import Data.Char
import Data.List

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap


main :: IO ()
main = do
  input <- lines <$> readFile "input14"
  print $ solve  input
  print $ solve2 input


type BitMask = (Int -> Int)


parseMask :: String -> BitMask
parseMask = make . drop 7 -- dropping the "mask = " prefix
  where
    make = combine . map singleBitBitMask . zip [0..] . reverse
    
    singleBitBitMask :: (Int, Char) -> BitMask 
    singleBitBitMask (pos, bit) =
      case bit of
        '1' -> (`setBit` pos)
        '0' -> (`clearBit` pos)
        'X'  -> id

    combine :: [BitMask] -> BitMask
    combine = foldl (.) id . reverse

    
parseMem :: String -> (Int, Int)
parseMem str =
  let a = takeWhile isDigit . drop 4 $ str
      b = takeWhile isDigit . drop 4 . dropWhile isDigit . drop 4 $ str
  in (read a, read b)


parseAll :: [String] -> IntMap Int
parseAll xs = go xs id IntMap.empty
  where
    go :: [String] -> BitMask -> IntMap Int -> IntMap Int
    go []     _    mem = mem
    go (x:xs) mask mem
      | "mask" `isPrefixOf` x = go xs (parseMask x) mem   
      | "mem"  `isPrefixOf` x = let (key, val) = parseMem x
                                    mval = mask val
                                    newMem = IntMap.insert key mval mem
                                in go xs mask newMem

                                    
solve :: [String] -> Int
solve = sum . parseAll

testInput :: [String]
testInput =
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
  ,"mem[8] = 11"
  ,"mem[7] = 101"
  ,"mem[8] = 0"]


-- part 2

makeMasks :: String -> [BitMask]
makeMasks str = foldl (liftM2 (.)) [constMask] floatMasks
                -- the lift is to the list monad
  where   
    floatMasks :: [[BitMask]]
    floatMasks = [ [(`setBit` i), (`clearBit` i)] | i <- floatingBits ]

    floatingBits :: [Int]
    floatingBits = [ pos | (pos, 'X') <- zip [0..] (reverse input) ]

    input :: String
    input = drop 7 str  -- dropping the "mask = " prefix

    constMask :: BitMask
    constMask = combine . map singleBitBitMask . zip [0..] . reverse $ input 

    singleBitBitMask :: (Int, Char) -> BitMask
    singleBitBitMask (pos, bit) = if bit == '1' then (`setBit` pos) else id
    
    combine :: [BitMask] -> BitMask
    combine = foldl (.) id . reverse


readAll2 :: [String] -> IntMap Int
readAll2 xs = go xs [id] IntMap.empty
  where
    go :: [String] -> [BitMask] -> IntMap Int -> IntMap Int
    go []     _     mem = mem
    go (x:xs) masks mem
      | "mask" `isPrefixOf` x = go xs (makeMasks x) mem 
      | "mem"  `isPrefixOf` x = let (key, val) = parseMem x
                                    keys = masks <*> [key]
                                    newMem = IntMap.fromList [(k, val) | k <- keys]
                                in go xs masks (IntMap.union newMem mem)
                                   -- Note: union is left-biased, it prefers the first
                                   -- map when duplicate keys are encountered

solve2 :: [String] -> Int
solve2 = sum . readAll2

testInput2 :: [String]
testInput2 =
  ["mask = 000000000000000000000000000000X1001X"
  ,"mem[42] = 100"
  ,"mask = 00000000000000000000000000000000X0XX"
  ,"mem[26] = 1"]
