import Data.Char
import qualified Data.Map.Strict as Map

import Intcode

type Column = Int
type Row = Int
type Point = (Row, Column)

main = do
    result <- run <$> readFile "input17.txt"
    print result

run program =
    let chart = parseInput . runToInput $ program
    in sumIntersections chart
    
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

runToInput = fmap (chr . fromIntegral) . programOutputs . runProgram . initializeProgram [] . parseProgram

-- 588 is too low
--
-- 8556 is too low
