
main :: IO ()
main = do
  input <- lines <$> readFile "input12"
  print $ solve  input
  print $ solve2 input



data Side      = L | R deriving (Eq)
data Direction = North | East | South | West deriving (Enum)

type Boat = ( Int        -- east/west position
            , Int        -- north/south position
            , Direction  -- orientation
            ) 

parseInstruction :: String -> (Boat -> Boat)
parseInstruction (c:cs) =
  let n = read cs :: Int
  in case c of
    'N' -> moveDirection North n 
    'E' -> moveDirection East  n
    'S' -> moveDirection South n
    'W' -> moveDirection West  n
    'F' -> moveForward n
    'L' -> turn L n
    'R' -> turn R n


moveDirection :: Direction -> Int -> Boat -> Boat
moveDirection dir n (x,y,o) =
  case dir of
    North  -> (x, y + n, o)
    East   -> (x + n, y, o)
    South  -> (x, y - n, o)
    West   -> (x - n, y, o)


moveForward :: Int -> Boat -> Boat
moveForward num (x, y, o) = moveDirection o num (x, y, o)


turn :: Side -> Int -> Boat -> Boat
turn side deg (x, y, o) = (x, y, newO)
  where
     newO = toEnum . (`mod` 4) . (`plusOrMinus` quarters) . fromEnum $ o
     plusOrMinus = if side == L then (-) else (+)
     quarters =
       case deg of
         90  -> 1
         180 -> 2
         270 -> 3


navigate :: [(a -> a)] -> (a -> a)
navigate ns = foldl (.) id (reverse ns)


solve :: [String] -> Int
solve input = (abs x) + (abs y)
 where
   (x,y,_) = navigate instructions $ (0,0,East)
   instructions = map parseInstruction input


testData :: [String]
testData = ["F10","N3","F7","R90","F11"]


-- part 2

data Boat' = Boat' Pos Waypoint
type Pos = (Int, Int)       -- east/west and north/south boat position
type Waypoint = (Int, Int)  -- east/west and north/south waypoint position



parseInstruction2 :: String -> (Boat' -> Boat')
parseInstruction2 (c:cs) =
  let n = read cs :: Int
  in case c of
    'N' -> moveWaypoint North n 
    'E' -> moveWaypoint East  n
    'S' -> moveWaypoint South n
    'W' -> moveWaypoint West  n
    'L' -> rotateWaypoint L n
    'R' -> rotateWaypoint R n
    'F' -> moveBoat n


moveWaypoint :: Direction -> Int -> Boat' -> Boat'
moveWaypoint dir n (Boat' pos (x,y)) = Boat' pos newWayPoint
  where
    newWayPoint =
      case dir of
        North  -> (x, y + n)
        East   -> (x + n, y)
        South  -> (x, y - n)
        West   -> (x - n, y)


rotateWaypoint :: Side -> Int -> Boat' -> Boat'
rotateWaypoint side degree ( Boat' pos wp ) = Boat' pos (rotate wp)
  where
    rotate = case degree of
               90  -> turn side
               180 -> turn side . turn side
               270 -> turn side . turn side . turn side
               
    turn :: Side -> Waypoint -> Waypoint
    turn L (x,y) = (-y, x)
    turn R (x,y) = (y, -x)


moveBoat :: Int -> Boat' -> Boat'
moveBoat num (Boat' (xp, yp) (x, y)) =
  let newPos = (xp + num * x, yp + num * y)
  in Boat' newPos (x,y)



solve2 :: [String] -> Int
solve2 input = (abs x) + (abs y)
 where  
   Boat' (x,y) _ = navigate instructions start
   start         = Boat' (0,0) (10,1)
   instructions  = map parseInstruction2 input
