import Control.Concurrent
import Data.Function
import Data.List
import qualified Data.Map as Map

import Intcode

main = do
    result <- run <$> readFile "input13.txt"
    print result
    runGame

run = Map.size . Map.filter (==2) . runMapProgram
run2 = printScreen . runMapProgram

runGame = do
    program <- withProgramMemory (Map.insert 0 2) . initializeProgram [] . parseProgram <$> readFile "input13.txt"
    loop program Map.empty 0
    return ()

loop program screen score = do
    let readyInput = nextInput [] program
    let outputs = programOutputs readyInput
    let (screen', score') = foldl (\(screen', score') (p@(row, column), tile) -> if row == -1 && column == 0 then (screen', tile) else (Map.insert p tile screen', score')) (screen, score) (extractPoints outputs)
    -- print score'
    -- putStrLn (printScreen screen')
    -- threadDelay (100 * 1000)
    -- c <- getChar
    let c = calculateMove screen'
    let joystick = if c == 'j' then 1 else if c == 'k' then -1 else 0
    if c `elem` "jks" && (not . Map.null . Map.filter (==2) $ screen')
        then loop (stepProgram . withProgramInputs (const [joystick]) $ readyInput) screen' score'
        else print score'

calculateMove :: Map.Map (Integer, Integer) Integer -> Char
calculateMove screen =
    let
        (ballRow, ballColumn) = head . Map.keys . Map.filter (==4) $ screen
        (paddleRow, paddleColumn) = head . Map.keys . Map.filter (==3) $ screen
    in
        if paddleColumn - ballColumn == 1 && ballRow == paddleRow
            then 's'
            else
                if paddleRow <= ballRow
                    then 'j'
                    else 'k'

runMapProgram = buildMap . programOutputs . runProgram . initializeProgram [] . parseProgram

buildMap :: [Integer] -> Map.Map (Integer, Integer) Integer
buildMap = Map.fromList . extractPoints

extractPoints = unfoldr (\points -> if null points then Nothing else Just (extract points, drop 3 points)) . reverse
    where
        extract (row:column:tile:_) = ((row, column), tile)

printScreen :: Map.Map (Integer, Integer) Integer -> String
printScreen = unlines . fmap (concat . fmap snd) . groupBy ((==) `on` (fst . fst)) . Map.toAscList . fmap showTile

showTile 0 = " "
showTile 1 = "#"
showTile 2 = "@"
showTile 3 = "|"
showTile 4 = "o"

-- not 15
