
import Data.List.Split
import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


main :: IO ()
main = do
  input <- lines <$> readFile "input21"
  print $ solve  input
  print $ solve2 input



type Ingredient = String
type Allergen  = String


{- input:

B C D E  (contains dairy, fish)
B F G H  (contains dairy)
D G      (contains soy)
B D H    (contains fish)


step 1: 

B C D E  (contains dairy)
B F G H  (contains dairy)
=> B contains dairy

B C D E  (contains fish)
B D H    (contains fish)
=> B D contains fish

D G      (contains soy)

Each input line (food listing) contains one or more hints to which ingredient
might contain which allergen. Split each food listing to hints for each allegen
and group them by allergen. Then, for one allergen we take the intersection of
all sets of ingerdients it might be contained in.
This is done by inserting the hints successievly into a dictionary with the
allergen as key and a set of ingredients as value, taking intersections upon
insertion. (See fuction `allergenInfo`)
The ingredients contained in the values of the dictionary are the ingredients
conatining an allergen, which solves the first part of the puzzle.



step 2:

B contains dairy
D contains fish
G contains soy

We continue taking intersections of the sets of ingredients until we
are left with just one ingredient per allergen. In the implementation it is
assumed, that at each step we can specify a new allergen ingredient pair. This
is necessary for an unique solution. 

-}



parse :: [String] -> [([Allergen],[Ingredient])]
parse input = zip allergens ingredients
  where
    allergens   = map ( splitOn ", " . init . drop 10 . dropWhile (/= '(') ) input 
    ingredients = map ( words . takeWhile (/= '(') ) input 


allergenInfo :: [String] -> Map Allergen (Set Ingredient)
allergenInfo input = foldr insertAllergen Map.empty hints
  where
    insertAllergen :: (Ord k, Ord a) => (k, Set a) -> Map k (Set a) -> Map k (Set a)
    insertAllergen = uncurry (Map.insertWith Set.intersection)

    hints :: [(Allergen, Set Ingredient)]
    hints = concatMap unpack . parse $ input
    
    unpack :: ([Allergen],[Ingredient]) -> [(Allergen, Set Ingredient)]
    unpack (as, bs) = [ (a, Set.fromList bs) | a <- as ]



solve :: [String] -> Int
solve input = count (`Set.member` ingredientsWithoutAllergens) allIngredients
  where
    allIngredients :: [Ingredient]
    allIngredients = concat . snd . unzip . parse $ input

    setOfIngredients :: Set Ingredient
    setOfIngredients = Set.fromList allIngredients

    ingredientsWithAllergens :: Set Ingredient
    ingredientsWithAllergens = foldl1 Set.union . allergenInfo $ input

    ingredientsWithoutAllergens :: Set Ingredient
    ingredientsWithoutAllergens =
      setOfIngredients `Set.difference` ingredientsWithAllergens
  


count :: (a -> Bool) -> [a] -> Int
count f = length . filter f 




-- part2



prune :: Map Allergen (Set Ingredient) -> Map Allergen (Set Ingredient)
prune xs = fmap (`removeElems` singletons) xs
  where
    singletons :: Set Ingredient
    singletons = Set.unions . Map.elems . Map.filter (\x -> Set.size x == 1) $ xs

    removeElem :: Set Ingredient -> Ingredient -> Set Ingredient
    removeElem set elem = if Set.size set == 1
                            then set
                            else Set.delete elem set

    removeElems :: Set Ingredient -> Set Ingredient -> Set Ingredient
    removeElems set elems = foldl' removeElem set elems


fullPrune :: Map Allergen (Set Ingredient) -> Map Allergen (Set Ingredient)
fullPrune xs = (iterate prune xs) !! Map.size xs


solve2 :: [String] -> String
solve2 = formatOutput . fullPrune . allergenInfo
  where
    formatOutput = concat . intersperse "," .
                   map ( head . Set.elems ) . 
                   Map.elems 
    


testInput =
  ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
  ,"trh fvjkl sbzzf mxmxvkd (contains dairy)"
  ,"sqjhc fvjkl (contains soy)"
  ,"sqjhc mxmxvkd sbzzf (contains fish)"]
