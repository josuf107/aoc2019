import Data.Bool
import Data.List
import Data.Maybe
import Data.Function
import Data.Tuple
import Data.Ord
import qualified Data.Map as Map

import Intcode

type Point = (Integer, Integer)
data Direction = DirUp | DirRight | DirDown | DirLeft deriving (Show, Eq, Ord, Enum)

data RobotState = RobotState { robotPoint :: Point, robotDirection :: Direction, colorMap :: Map.Map Point Integer } deriving (Show, Eq, Ord)

withRobotPoint f s = s { robotPoint = f . robotPoint $ s }
withRobotDirection f s = s { robotDirection = f . robotDirection $ s }
withColorMap f s = s { colorMap = f . colorMap $ s }

main = do
    result <- run <$> readFile "input11.txt"
    print result
    result2 <- run2 <$> readFile "input11.txt"
    putStrLn result2

run input = Map.size . colorMap . snd $ runOver (initializeProgram [] . parseProgram $ input)
run2 input = printGridMap . colorMap . snd $ runOver2 (initializeProgram [] . parseProgram $ input)

printGridMap :: Map.Map Point Integer -> String
printGridMap gridMap = unlines . fmap (concat . fmap snd) . groupBy ((==) `on` (fst . fst)) . Map.toAscList . fmap showPoint $ gridMap

showPoint 1 = "B"
showPoint _ = " "

runOver program = until (endProgram . fst) (uncurry step) (program, initialState)
runOver2 program = until (endProgram . fst) (uncurry step) (program, withColorMap (Map.insert (0, 0) 1) initialState)

step :: Program -> RobotState -> (Program, RobotState)
step program state =
    let
        program' = nextOutput [(getColor (colorMap state) (robotPoint state))] program
    in
        if endProgram program'
            then (program', state)
            else
                let
                    nextColor = head . programOutputs $ program'
                    nextProgram = nextOutput [] program'
                    nextDirection = head . programOutputs $ nextProgram
                in
                    (nextProgram, bool (updateRight nextColor state) (updateLeft nextColor state) (nextDirection == 0))

initialState = RobotState (0, 0) DirUp Map.empty

updateLeft color = advance . withRobotDirection turnLeft . setColor color
updateRight color = advance . withRobotDirection turnRight . setColor color

setColor color state = withColorMap (Map.insert (robotPoint state) color) state

advance s = withRobotPoint (move (robotDirection s)) s

turnLeft DirUp = DirLeft
turnLeft d = pred d

turnRight DirLeft = DirUp
turnRight d = succ d

move d (x, y) =
    case d of
        DirUp -> (x, y + 1)
        DirRight -> (x + 1, y)
        DirDown -> (x, y - 1)
        DirLeft -> (x - 1, y)

getColor colors point = fromMaybe 0 $ Map.lookup point colors
