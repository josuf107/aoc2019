import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree

import Intcode

type Position = (Integer, Integer)

data RobotState = RobotState
    { robotChart :: Map.Map Position Integer
    , robotProgram :: Program
    , robotPosition :: Position
    , robotHistory :: [Integer]
    } deriving (Show)

type StateWith a = (a -> a) -> State RobotState ()

withRobotChart :: StateWith (Map.Map Position Integer)
withRobotChart f = modify (\r -> r { robotChart = f . robotChart $ r })

withRobotProgram :: StateWith Program
withRobotProgram f = modify (\r -> r { robotProgram = f . robotProgram $ r })

withRobotPosition :: StateWith Position
withRobotPosition f = modify (\r -> r { robotPosition = f . robotPosition $ r })

withRobotHistory :: StateWith [Integer]
withRobotHistory f = modify (\r -> r { robotHistory = f . robotHistory $ r })

main = do
    result <- run <$> readFile "input15.txt"
    print result

run program =
    let
        chart = getChart program
        oxygen = head . Map.keys . Map.filter (==2) $ chart
        searchTree = evalState (unfoldTreeM (buildTree chart) (0, 0)) Set.empty
        result1 = length . takeWhile (oxygen `notElem`) . levels $ searchTree
        oxygenTree = evalState (unfoldTreeM (buildTree chart) oxygen) Set.empty
        result2 = subtract 1 . length . levels $ oxygenTree
    in (result1, result2)
    
buildTree :: Map.Map Position Integer -> Position -> State (Set.Set Position) (Position, [Position])
buildTree chart point = do
    modify (Set.insert point)
    seen <- flip Set.member <$> get
    let nextPoints = fmap (flip move point) [1..4]
    let validPoint p' = Map.lookup p' chart `elem` [Just 1, Just 2] && not (seen p')
    return (point, filter validPoint nextPoints)

getChart program = robotChart $ execState makeChart (initialState program)

initialState program = RobotState
    Map.empty
    (initializeProgram [] . parseProgram $ program)
    (0, 0)
    []

makeChart = do
    maybeMove <- nextUnknown
    case maybeMove of
        Just nextMove -> do
            result <- goDirection nextMove
            when (result /= 0) (withRobotHistory (nextMove:))
            makeChart
        Nothing -> do
            done <- backtrack
            unless done makeChart

nextUnknown :: State RobotState (Maybe Integer)
nextUnknown = do
    position <- robotPosition <$> get
    chart <- robotChart <$> get
    return (openPosition position chart)
    where
        openPosition position chart = listToMaybe
            . filter (\direction -> not . Map.member (move direction position) $ chart)
            $ [1, 2, 3, 4]

goDirection :: Integer -> State RobotState Integer
goDirection direction = do
    withRobotProgram (nextOutput [direction])
    result <- head . programOutputs . robotProgram <$> get
    position <- move direction . robotPosition <$> get
    when (result /= 0) (withRobotPosition . const $ position)
    withRobotChart . Map.insert position $ result
    return result

backtrack = do
    history <- robotHistory <$> get
    case history of
        [] -> return True
        (h:hs) -> do
            goDirection (retrace h)
            withRobotHistory . const $ hs
            nextDirection <- nextUnknown
            case nextDirection of
                Nothing -> backtrack
                Just _ -> return False

move d (x, y) = case d of
    1 -> (x, y + 1)
    2 -> (x, y - 1)
    3 -> (x - 1, y)
    4 -> (x + 1, y)

retrace d = case d of
    1 -> 2
    2 -> 1
    3 -> 4
    4 -> 3
