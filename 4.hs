
import Data.List
import Data.List.Split (splitOn)
import Data.Char


main :: IO ()
main = do
  input <- lines <$> readFile "input4"
  let  solution2 = length . filter id $ map valid2 $ readPassports input
  print $ solve valid  input 
  print $ solve valid2 input  


type Key = String
type Value = String
type PassportData = [(Key, Value)]


readPassports :: [String] -> [PassportData]
readPassports =
  map (map (splitAt 4)) .
  map concat .
  splitOn [[]] .
  map words


valid :: PassportData -> Bool
valid pd =
  case length pd of
    8                                -> True
    7 | Nothing <- lookup "cid:" pd  -> True
    otherwise                        -> False


solve :: (PassportData -> Bool) -> [String] ->  Int
solve validator input = count True . map validator $ passports
  where
    passports = readPassports input
    count x = length . filter (==x)


-- part 2


inRange :: Int -> (Int, Int) -> Bool
inRange num (low, high) =
  low <= num && num <= high
  

byr, iyr, eyr :: Value -> Bool
byr s = read s `inRange` (1920, 2002)
iyr s = read s `inRange` (2010, 2020)
eyr s = read s `inRange` (2020, 2030)


hgt :: Value -> Bool
hgt s =
  let num  = read $ takeWhile isDigit s :: Int
      unit = dropWhile isDigit s
      
  in case unit of
       "cm"      -> num `inRange` (150, 193)
       "in"      -> num `inRange` (59, 76)
       otherwise -> False 


hcl :: Value -> Bool
hcl ('#':rest) = length rest == 6 && all isHexDigit rest
hcl _ = False


ecl :: Value -> Bool
ecl s = s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]


pid :: Value -> Bool 
pid s = all isDigit s && length s == 9


cid :: Value -> Bool
cid s = True 


valid2 :: PassportData -> Bool
valid2 pd = (length values == 7) && (and $ zipWith ($) tests values)
 where
   tests = [byr, ecl, eyr, hcl, hgt, iyr, pid] -- in alphabetical order
      
   values = snd . unzip .  -- pick just the values 
            sortOn fst .   -- sort the password data list by key
            ignoreCid $ pd
               
   ignoreCid = filter (\x -> fst x /= "cid:")
   



testInput :: [String]
testInput =
  ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  ,"byr:1937 iyr:2017 cid:147 hgt:183cm"
  ,""
  ,"iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
  ,"hcl:#cfa07d byr:1929"
  ,""
  ,"hcl:#ae17e1 iyr:2013"
  ,"eyr:2024"
  ,"ecl:brn pid:760753108 byr:1931"
  ,"hgt:179cm"
  ,""
  ,"hcl:#cfa07d eyr:2025 pid:166559648"
  ,"iyr:2011 ecl:brn hgt:59in"
  ]


testInputInvalid :: [String]
testInputInvalid =
  ["eyr:1972 cid:100"
  ,"hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
  ,""
  ,"iyr:2019"
  ,"hcl:#602927 eyr:1967 hgt:170cm"
  ,"ecl:grn pid:012533040 byr:1946"
  ,""
  ,"hcl:dab227 iyr:2012"
  ,"ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
  ,""
  ,"hgt:59cm ecl:zzz"
  ,"eyr:2038 hcl:74454a iyr:2023"
  ,"pid:3556412378 byr:2007"
  ]


testInputValid :: [String]
testInputValid =
  ["pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980"
  ,"hcl:#623a2f"
  ,""
  ,"eyr:2029 ecl:blu cid:129 byr:1989"
  ,"iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
  ,""
  ,"hcl:#888785"
  ,"hgt:164cm byr:2001 iyr:2015 cid:88"
  ,"pid:545766238 ecl:hzl"
  ,"eyr:2022"
  ,""
  ,"iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  ]
