import Control.Monad.State as State
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Tree
import Text.ParserCombinators.ReadP as ReadP
import qualified Data.Map.Strict as Map

main = do
    result <- run <$> readFile "input14.txt"
    print result
    result2 <- run2 <$> readFile "input14.txt"
    print result2

data Recipe = Recipe { recipeIngredients :: [(Integer, String)], recipeYield :: Integer }
    deriving (Show, Eq, Ord)

run2 input =
    let
        cookbook = parseCookbook input
    in last . levels . walkTree cookbook
        $ unfoldTree
            (\range@(low, high) -> (range, splitRange low high))
            (initialRange cookbook)

splitRange low high = let newSize = (high - low) `div` 2 in [(low, low + newSize), (low + newSize, high)]

walkTree :: Map.Map String Recipe -> Tree (Integer, Integer) -> Tree (Integer, Integer)
walkTree cookbook (Node range@(low, high) nodes) =
    if high - low < 2
        then Node range []
    else if fuelInRange cookbook range
        then Node range (fmap (walkTree cookbook) nodes)
    else Node range []

initialRange cookbook = head . filter (fuelInRange cookbook) $ zip powersOfTwo (tail powersOfTwo)
    where
        powersOfTwo = fmap (2^) [0..]

fuelInRange cookbook (low, high) =
    oreForFuel cookbook low <= 1000000000000
    && oreForFuel cookbook high > 1000000000000

run input =
    let cookbook = parseCookbook input
    in oreForFuel cookbook 1

oreForFuel cookbook fuel = countOre $ evalState (buildTree cookbook fuel) Map.empty

countOre = sum . fmap fst . filter ((=="ORE") . snd) . toList

buildTree :: Map.Map String Recipe -> Integer -> State (Map.Map String Integer) (Tree (Integer, String))
buildTree cookbook fuel = unfoldTreeM (stepRecipe cookbook) (fuel, "FUEL")

stepRecipe :: Map.Map String Recipe -> (Integer, String) -> State (Map.Map String Integer) ((Integer, String), [(Integer, String)])
stepRecipe _ target@(_, "ORE") = return (target, [])
stepRecipe cookbook target@(amount, chemical) = do
    leftovers <- fromMaybe 0 . Map.lookup chemical <$> State.get
    let (amountNeeded, stillLeftover) = if leftovers >= amount then (0, leftovers - amount) else (amount - leftovers, 0)
    modify (Map.insert chemical stillLeftover)
    if amountNeeded == 0
        then return (target, [])
        else do
            let recipe = cookbook Map.! chemical
            let (evenBatches, remainder) = amountNeeded `divMod` recipeYield recipe
            let (batches, additionalLeftover) = if remainder > 0 then (evenBatches + 1, recipeYield recipe - remainder) else (evenBatches, 0)
            modify (Map.insertWith (+) chemical additionalLeftover)
            let needs = fmap (\(ingredientAmount, ingredient) -> (ingredientAmount * batches, ingredient)) . recipeIngredients $ recipe
            return (target, needs)

parseCookbook = Map.fromList . fmap (\(ingredients, (amount, result)) -> (result, Recipe ingredients amount)) . parse

parse = fst . head . (readP_to_S inputp)

inputp :: ReadP [([(Integer, String)], (Integer, String))]
inputp = equationp `sepBy` (char '\n') <* (skipSpaces >> eof)

equationp = do
    ingredients <- ingredientp `sepBy` (string ", ")
    string " => "
    result <- ingredientp
    return (ingredients, result)

ingredientp = do
    amount <- read <$> many1 (satisfy isDigit)
    char ' '
    chemical <- many1 (satisfy isAlpha)
    return (amount, chemical)
