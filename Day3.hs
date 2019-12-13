import Data.Char
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP as ReadP
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Vector = (Int, Int)

test1 = "R8,U5,L5,D3\nU7,R6,D4,L4"
test2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
test3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

main = do
    input <- readFile "input3.txt"
    print (day3 input)
    print (day32 input)

day3 = run . readInput
day32 = run2 . readInput

run (wire1, wire2) = (\(x, y) -> abs x + abs y) $ findMinIntersection wire1 wire2

run2 (wire1, wire2) = findMinDistance wire1 wire2

findMinDistance wire1 wire2 = head . sort . fmap snd . Map.toList $ intersectingDistances wire1 wire2

intersectingDistances wire1 wire2 = Map.intersectionWith (+) (traceWireDistances wire1) (traceWireDistances wire2)

findMinIntersection :: [Vector] -> [Vector] -> Vector
findMinIntersection wire1 wire2 = minimumBy (comparing (\(x, y) -> (abs x + abs y))) $ intersectingPoints wire1 wire2

intersectingPoints :: [Vector] -> [Vector] -> [Vector]
intersectingPoints wire1 wire2 = Set.toList . Set.delete (0, 0) $ Set.intersection (traceWire wire1) (traceWire wire2)

traceWireDistances :: [Vector] -> Map.Map Vector Int
traceWireDistances = (\(_, _, m) -> m) . foldl' addPathDistance (0, (0, 0), Map.empty)

addPathDistance :: (Int, Vector, Map.Map Vector Int) -> Vector -> (Int, Vector, Map.Map Vector Int)
addPathDistance (step, (x, y), distances) (dx, dy) =
    let
        newPoint = (x + dx, y + dy)
        xs = [x..fst newPoint] ++ [fst newPoint..x]
        ys = [y..snd newPoint] ++ [snd newPoint..y]
        newPoints = nub . sortBy (comparing (dist (x, y))) $ [(x', y') | x' <- xs, y' <- ys, (x', y') /= (x, y)]
        newDistances = zip newPoints [step + 1..]
    in (step + length newPoints, newPoint, Map.unionWith min distances (Map.fromList newDistances))

dist :: Vector -> Vector -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

traceWire :: [Vector] -> Set.Set Vector
traceWire = snd . foldl' addPath ((0, 0), Set.empty)

addPath :: (Vector, Set.Set Vector) -> Vector -> (Vector, Set.Set Vector)
addPath ((x, y), pointSet) (dx, dy) =
    let
        newPoint = (x + dx, y + dy)
        xs = [x..fst newPoint] ++ [fst newPoint..x]
        ys = [y..snd newPoint] ++ [snd newPoint..y]
        newPoints = [(x', y') | x' <- xs, y' <- ys]
    in (newPoint, Set.union pointSet (Set.fromList newPoints))

readInput :: String -> ([Vector], [Vector])
readInput = fst . head . ReadP.readP_to_S parseInput

parseInput :: ReadP ([Vector], [Vector])
parseInput = do
    d1 <- parseDirections
    char '\n'
    d2 <- parseDirections
    skipSpaces
    eof
    return (d1, d2)

parseDirections :: ReadP [Vector]
parseDirections = sepBy parseDirection (char ',')

parseDirection :: ReadP Vector
parseDirection = do
    direction <- ReadP.get
    amount <- read <$> many1 (satisfy isDigit)
    return $ case direction of
        'R' -> (amount, 0)
        'L' -> (negate amount, 0)
        'U' -> (0, amount)
        'D' -> (0, negate amount)
