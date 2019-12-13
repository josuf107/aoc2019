import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import qualified Data.Map as Map
import qualified Data.Set as Set

main = do
    result <- run <$> readFile "input10.txt"
    print result
    result2 <- run2 <$> readFile "input10.txt"
    print result2

type Point = (Int, Int)
data Line = LineFunction (Ratio Int) (Ratio Int) | Vertical Int deriving (Ord, Eq, Show)
type PointSet = Set.Set Point

run input = 
    let
        points = parse input
    in maximum . fmap length . fmap (\p -> reachableFrom p points) $ points

run2 input =
    let
        points = parse input
        station = stationPoint points
    in (\(row, column) -> column * 100 + row) . (!!199) . concat $ unfoldr (sweepLaser station) (Set.fromList points)

sweepLaser station remainingPoints =
    if null remainingPoints
        then Nothing
        else
            let destroyedPoints = nextRound station remainingPoints
            in Just (destroyedPoints, Set.difference remainingPoints (Set.fromList destroyedPoints))

nextRound station = sortBy (comparing (radiansFrom station)) . reachableFrom station . Set.toList
    
stationPoint points = fst . maximumBy (comparing (length . snd)) . fmap (\p -> (p, reachableFrom p points)) $ points

radiansFrom :: Point -> Point -> Double
radiansFrom (x1, y1) (x2, y2) =
    let
        radiansFromXAxis = atan2 (fromIntegral $ x2 - x1) (fromIntegral $ y2 - y1) + (pi / 2)
    in
        if radiansFromXAxis < 0 then radiansFromXAxis + 2 * pi else radiansFromXAxis

reachableFrom :: Point -> [Point] -> [Point]
reachableFrom p points = fold . fmap (\ps -> catMaybes [Set.lookupGT p ps, Set.lookupLT p ps]) $ linesFrom p points

linesFrom :: Point -> [Point] -> Map.Map Line PointSet
linesFrom p = Map.fromListWith (Set.union) . fmap (\p2 -> let line = lineBetween p p2 in (line, Set.fromList [p, p2]))

lineBetween (x1, y1) (x2, y2) =
    let
        a = (y1 - y2) % (x1 - x2)
        b = (fromIntegral y1) - (a * (fromIntegral x1))
    in if x1 == x2 then Vertical x1 else LineFunction a b

parse = fmap fst
    . filter ((=='#') . snd)
    . concat
    . fmap (\(row, cells) -> fmap (\(column, cell) -> ((row, column), cell)) . zip [0..] $ cells)
    . zip [0..]
    . lines
