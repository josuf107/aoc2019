import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map.Strict as Map

import Intcode

data Direction = North | South | East | West deriving (Show, Eq, Ord)

main = do
    result <- run <$> readFile "input17.txt"
    print result
    result2 <- run2 <$> readFile "input17.txt"
    print result2

run program =
    let chart = parseInput . runToInput $ program
    in sumIntersections chart

run2 program =
    let
    in runRobot (writeProgram program) program

writeProgram program =
    let
        chart = parseInput . runToInput $ program
        path = makePath chart
        [al, bl, cl] = getProgramPrefixes path
        a = take al path
        b = take bl (removePrefixes [al] path)
        c = take cl (removePrefixes [al, bl] path)
    in unlines $
        [intersperse ',' (writeMain a b c path)]
        ++ fmap (intercalate ",") [a, b, c]
        ++ ["n"]

writeMain _ _ _ [] = []
writeMain a b c path =
    let
        (nextFunctionName, nextFunction) = head . filter ((`isPrefixOf` path) . snd) . zip "ABC" $ [a, b, c]
    in nextFunctionName : writeMain a b c (drop (length nextFunction) path)

getProgramPrefixes p =
    let
        possiblePrefixes = do
            a <- [1..20]
            b <- [1..20]
            c <- [1..20]
            return [a, b, c]
    in
        head . filter (\l -> null . removePrefixes l $ p) . sortBy (comparing sum) $ possiblePrefixes

removePrefixes ns l = foldl (\l' n -> removePrefix n l') l ns

removePrefix n l = removeSublist (take n l) l

removeSublist _ [] = []
removeSublist piece whole =
    if and . zipWith (==) piece $ whole
        then removeSublist piece (drop (length piece) whole)
        else head whole : removeSublist piece (drop 1 whole)

makePath chart =
    let
        start = head . Map.keys . Map.filter (=='^') $ chart
        startDirection = North
        path = unfoldr (\(position, direction) -> fmap (\r -> (r, r)) $ nextStep chart position direction) (start, North)
        directions = makeDirections North . fmap snd $ path
    in directions

makeDirections oldDirection [] = []
makeDirections oldDirection allDirections@(newDirection:directions) =
    let
        forwardLength = show . length . takeWhile (==oldDirection) $ allDirections
    in
        if newDirection == oldDirection
            then forwardLength : makeDirections oldDirection (dropWhile (==oldDirection) allDirections)
            else [turnLetter oldDirection newDirection] : makeDirections newDirection allDirections

turnLetter old new =
    if (old, new) `elem` [(North, East), (East, South), (South, West), (West, North)]
        then 'R'
        else 'L'

nextStep chart position direction =
    let
        isOpen p = Map.lookup p chart == Just '#'
        directions = [direction, North, South, East, West]
        available d =  and [isOpen . move position $ d, d /= opposite direction]
        direction' = listToMaybe . filter available $ directions
    in case direction' of
        Just d -> Just (move position d, d)
        Nothing -> Nothing

move (row, column) direction =
    case direction of
        North -> (row - 1, column)
        South -> (row + 1, column)
        East -> (row, column + 1)
        West -> (row, column - 1)

opposite direction = case direction of
    North -> South
    South -> North
    East -> West
    West -> East

sumIntersections chart = sum . fmap (uncurry (*)) . findIntersections $ chart
findIntersections chart = filter (isIntersection chart) . Map.keys $ chart

isIntersection chart p@(row, column) =
    let isScaffold p' = Map.lookup p' chart == Just '#'
    in all isScaffold [p, (row - 1, column), (row + 1, column), (row, column - 1), (row, column + 1)]

parseInput = Map.fromList
    . concat 
    . fmap (\(rowNum, row) -> fmap (\(colNum, cell) -> ((rowNum, colNum), cell)) . zip [0..] $ row) 
    . zip [0..] 
    . filter (not . null) 
    . lines

runToInput = fmap (chr . fromIntegral) . reverse . programOutputs . runProgram . initializeProgram [] . parseProgram

runRobot input = head . programOutputs . runProgram . withProgramMemory (Map.insert 0 2) . initializeProgram (fmap (fromIntegral . ord) input) . parseProgram
